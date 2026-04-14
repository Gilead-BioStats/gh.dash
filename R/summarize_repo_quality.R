#' Count test_that calls in decoded R source
#'
#' @param source_text Character string with R code
#' @return Integer number of actual `test_that()` calls
#' @keywords internal
#' @noRd
count_test_that_calls <- function(source_text) {
  if (is.null(source_text) || !length(source_text)) {
    return(0L)
  }

  source_text <- paste(as.character(source_text), collapse = "\n")
  if (!nzchar(source_text)) {
    return(0L)
  }

  expr <- tryCatch(
    parse(text = source_text, keep.source = TRUE),
    error = function(e) NULL
  )
  if (is.null(expr)) {
    return(0L)
  }

  parse_data <- utils::getParseData(expr)
  if (is.null(parse_data) || !nrow(parse_data)) {
    return(0L)
  }

  as.integer(sum(
    parse_data$token == "SYMBOL_FUNCTION_CALL" &
      parse_data$text == "test_that"
  ))
}

#' Summarize repository quality signals
#'
#' Internal function that computes package-level quality indicators:
#' the number of `test_that()` tests and whether qcthat workflow is present.
#'
#' @param repos Character vector of repositories in the form `owner/repo`.
#' @param token Optional GitHub personal access token.
#' @param releases_cache Optional named list of pre-fetched releases, keyed by
#'   `"owner/repo"`. When an entry is present for a repo, the internal
#'   `fetch_releases()` call is skipped.
#' @return Data frame with columns `repo`, `test_count`, and `qcthat_status`.
#' @keywords internal
#' @noRd
summarize_repo_quality <- function(repos, token = NULL, releases_cache = NULL) {
  validate_repo_vector(repos)

  results <- vector("list", length(repos))

  for (idx in seq_along(repos)) {
    owner_repo <- repos[[idx]]
    repo_parts <- split_repo_slug(owner_repo)
    owner <- repo_parts$owner
    repo <- repo_parts$repo

    metadata <- fetch_repo_metadata(owner, repo, token)
    default_branch <- "HEAD"
    is_private <- FALSE
    if (!is.null(metadata)) {
      default_branch <- metadata$default_branch %||% "HEAD"
      is_private <- metadata$private %||% FALSE
    }

    tree <- fetch_repo_git_tree(owner, repo, default_branch, token)

    if (is.null(tree)) {
      # API failure (rate-limit or permission error) — surface NA rather than
      # conflating unavailability with a genuinely empty repo.
      results[[idx]] <- list(
        repo = format_repo_link(owner, repo, is_private = is_private),
        coverage = "Unavailable",
        test_count = NA_integer_,
        qcthat_status = "Unavailable"
      )
      next
    }

    if (isTRUE(tree$truncated)) {
      # Tree was truncated by GitHub (repo too large for a single recursive call);
      # tree-based results (test count, qcthat) are incomplete, but coverage is
      # fetched from releases via a separate API endpoint and is still available.
      coverage_data <- fetch_coverage_percent(owner, repo, token, releases = releases_cache[[owner_repo]])
      results[[idx]] <- list(
        repo = format_repo_link(owner, repo, is_private = is_private),
        coverage = format_coverage_summary(coverage_data$coverage_percent, coverage_data$release_url),
        test_count = NA_integer_,
        qcthat_status = "Unavailable"
      )
      next
    }

    tree_entries <- tree$tree

    if (is.null(tree_entries) || !length(tree_entries)) {
      test_count <- 0L
      has_qcthat <- FALSE
    } else {
      tree_paths <- vapply(tree_entries, function(entry) entry$path %||% "", character(1))

      has_qcthat <- any(tree_paths == ".github/workflows/qcthat.yaml")

      test_file_paths <- tree_paths[
        startsWith(tree_paths, "tests/testthat/") & grepl("\\.[Rr]$", tree_paths)
      ]

      if (!length(test_file_paths)) {
        test_count <- 0L
      } else {
        # Cap at 50 files to limit API usage and avoid rate-limit issues.
        max_test_files <- 50L
        test_file_paths <- utils::head(test_file_paths, max_test_files)
        test_total <- 0L
        any_fetch_failed <- FALSE
        for (path in test_file_paths) {
          content <- fetch_repo_file_content(owner, repo, path, default_branch, token)
          if (is.null(content) || is.null(content$content) || !length(content$content)) {
            any_fetch_failed <- TRUE
            next
          }
          encoded <- content$content

          decoded <- decode_base64_string(encoded)
          test_total <- test_total + count_test_that_calls(decoded)
        }
        # If any file could not be fetched, the count is incomplete; surface NA.
        test_count <- if (any_fetch_failed) NA_integer_ else as.integer(test_total)
      }
    }

    coverage_data <- fetch_coverage_percent(owner, repo, token, releases = releases_cache[[owner_repo]])

    results[[idx]] <- list(
      repo = format_repo_link(owner, repo, is_private = is_private),
      coverage = format_coverage_summary(coverage_data$coverage_percent, coverage_data$release_url),
      test_count = as.integer(test_count),
      qcthat_status = if (isTRUE(has_qcthat)) "Yes" else "No"
    )
  }

  data.frame(
    repo = vapply(results, `[[`, character(1), "repo"),
    coverage = vapply(results, `[[`, character(1), "coverage"),
    test_count = vapply(results, `[[`, integer(1), "test_count"),
    qcthat_status = vapply(results, `[[`, character(1), "qcthat_status"),
    stringsAsFactors = FALSE
  )
}

#' Fetch coverage percentage from latest release asset
#'
#' Looks for a `coverage-summary.json` asset in the latest non-draft,
#' non-prerelease GitHub release and parses `coverage_percent` from it.
#'
#' @param owner Repository owner (GitHub username or organization)
#' @param repo Repository name
#' @param token GitHub personal access token (optional)
#' @param releases Optional pre-fetched releases list for this repo. When
#'   supplied, skips the internal `fetch_releases()` call.
#' @return List with `coverage_percent` (numeric or NA) and `release_url`
#'   (character or NULL)
#' @keywords internal
#' @noRd
fetch_coverage_percent <- function(owner, repo, token, releases = NULL) {
  na_result <- list(coverage_percent = NA_real_, release_url = NULL)

  if (is.null(releases)) {
    releases <- fetch_releases(owner, repo, token)
  }
  latest <- derive_latest_release(releases)

  if (is.null(latest)) {
    return(na_result)
  }

  release_url <- latest$html_url %||% NULL
  result <- list(coverage_percent = NA_real_, release_url = release_url)

  assets <- latest$assets
  if (is.null(assets) || !length(assets)) {
    return(result)
  }

  asset <- NULL
  for (a in assets) {
    if (identical(a$name, "coverage-summary.json")) {
      asset <- a
      break
    }
  }

  if (is.null(asset)) {
    return(result)
  }

  raw <- fetch_release_asset_content(asset$url, token)
  if (is.null(raw) || !nzchar(raw)) {
    return(result)
  }

  parsed <- tryCatch(
    jsonlite::fromJSON(raw, simplifyVector = TRUE),
    error = function(e) NULL
  )

  if (is.null(parsed) || is.null(parsed$coverage_percent)) {
    return(result)
  }

  pct <- suppressWarnings(as.numeric(parsed$coverage_percent))
  result$coverage_percent <- if (length(pct) == 1L && is.finite(pct)) pct else NA_real_
  result
}

#' Format coverage percentage as HTML
#'
#' Returns a linked percentage string (e.g. "84.7%") or "Unavailable" when
#' no coverage data is available.
#'
#' @param coverage_percent Numeric coverage percentage, or NA
#' @param release_url Character URL of the release page, or NULL
#' @return Character HTML string
#' @keywords internal
#' @noRd
format_coverage_summary <- function(coverage_percent, release_url) {
  if (is.na(coverage_percent)) {
    return("Unavailable")
  }

  label <- sprintf("%.1f%%", coverage_percent)

  if (!is.null(release_url) && nzchar(release_url)) {
    as.character(htmltools::tags$a(
      href = release_url,
      target = "_blank",
      rel = "noopener noreferrer",
      label
    ))
  } else {
    label
  }
}
