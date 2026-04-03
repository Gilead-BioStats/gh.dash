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
#' @return Data frame with columns `repo`, `test_count`, and `qcthat_status`.
#' @keywords internal
#' @noRd
summarize_repo_quality <- function(repos, token = NULL) {
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
        for (path in test_file_paths) {
          content <- fetch_repo_file_content(owner, repo, path, default_branch, token)
          if (is.null(content) || is.null(content$content) || !length(content$content)) {
            next
          }
          encoded <- content$content

          decoded <- decode_base64_string(encoded)
          test_total <- test_total + count_test_that_calls(decoded)
        }
        test_count <- as.integer(test_total)
      }
    }

    results[[idx]] <- list(
      repo = format_repo_link(owner, repo, is_private = is_private),
      test_count = as.integer(test_count),
      qcthat_status = if (isTRUE(has_qcthat)) "Yes" else "No"
    )
  }

  data.frame(
    repo = vapply(results, `[[`, character(1), "repo"),
    test_count = vapply(results, `[[`, integer(1), "test_count"),
    qcthat_status = vapply(results, `[[`, character(1), "qcthat_status"),
    stringsAsFactors = FALSE
  )
}
