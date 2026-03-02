#' Fetch releases from GitHub API
#'
#' Internal function to retrieve published releases for a GitHub repository.
#' Returns up to 100 releases per page. Used to compute year-to-date release
#' counts while reusing the same response for latest-release information.
#'
#' @param owner Repository owner (GitHub username or organization)
#' @param repo Repository name
#' @param token GitHub personal access token (optional)
#' @return List of release objects or NULL if none found
#' @keywords internal
#' @importFrom gh gh
fetch_releases <- function(owner, repo, token) {
  safe_gh(
    gh::gh,
    "GET /repos/{owner}/{repo}/releases",
    owner = owner,
    repo = repo,
    per_page = 100,
    .token = token
  )
}

#' Fetch open milestones from GitHub API
#'
#' Internal function to retrieve open milestones for a GitHub repository.
#'
#' @param owner Repository owner (GitHub username or organization)
#' @param repo Repository name
#' @param token GitHub personal access token (optional)
#' @return List of milestone objects or NULL if none found
#' @keywords internal
#' @importFrom gh gh
fetch_open_milestones <- function(owner, repo, token) {
  safe_gh(
    gh::gh,
    "GET /repos/{owner}/{repo}/milestones",
    owner = owner,
    repo = repo,
    state = "open",
    per_page = 100,
    .token = token
  )
}

#' Fetch open pull requests count from GitHub API
#'
#' Internal function to retrieve count of open pull requests for a GitHub repository.
#'
#' @param owner Repository owner (GitHub username or organization)
#' @param repo Repository name
#' @param token GitHub personal access token (optional)
#' @return Integer count of open PRs or NULL if retrieval fails
#' @keywords internal
#' @importFrom gh gh
fetch_open_prs <- function(owner, repo, token) {
  result <- safe_gh(
    gh::gh,
    "GET /repos/{owner}/{repo}/pulls",
    owner = owner,
    repo = repo,
    state = "open",
    .token = token
  )
  
  # Extract the count from response headers if available
  if (!is.null(result)) {
    headers <- attr(result, "response")
    if (!is.null(headers)) {
      link_header <- headers$headers[["link"]]
      if (!is.null(link_header) && nzchar(link_header)) {
        # Parse the Link header to get total count
        # Format: <url>; rel="last"
        parts <- strsplit(link_header, ",")[[1]]
        for (part in parts) {
          if (grepl('rel="last"', part)) {
            # Extract page number from the last URL
            match <- regexpr("page=([0-9]+)", part)
            if (match > 0) {
              page_str <- regmatches(part, regexpr("[0-9]+", substring(part, regexpr("page=", part))))
              if (length(page_str) > 0) {
                last_page <- as.integer(page_str[1])
                # Return (last_page - 1) * per_page + items_on_last_page
                # But we don't know items on last page easily, so count the current results
                return(length(result) + (last_page - 1) * 30)
              }
            }
          }
        }
      }
    }
    # If no Link header, just return the count of results
    return(length(result))
  }
  
  0
}

#' Fetch repository metadata from GitHub API
#'
#' Internal function to retrieve basic repository metadata including private status.
#'
#' @param owner Repository owner (GitHub username or organization)
#' @param repo Repository name
#' @param token GitHub personal access token (optional)
#' @return List with repository metadata or NULL if retrieval fails
#' @keywords internal
#' @importFrom gh gh
fetch_repo_metadata <- function(owner, repo, token) {
  safe_gh(
    gh::gh,
    "GET /repos/{owner}/{repo}",
    owner = owner,
    repo = repo,
    .token = token
  )
}

#' Fetch branch comparison from GitHub API
#'
#' Internal function to compare two branches in a GitHub repository.
#'
#' @param owner Repository owner (GitHub username or organization)
#' @param repo Repository name
#' @param base Base branch for comparison
#' @param head Head branch for comparison
#' @param token GitHub personal access token (optional)
#' @return List with comparison information or NULL if comparison fails
#' @keywords internal
#' @importFrom gh gh
fetch_branch_comparison <- function(owner, repo, base, head, token) {
  safe_gh(
    gh::gh,
    "GET /repos/{owner}/{repo}/compare/{base}...{head}",
    owner = owner,
    repo = repo,
    base = base,
    head = head,
    .token = token
  )
}

#' Fetch pull requests updated within a lookback window
#'
#' Internal function to retrieve pull requests sorted by update time and keep
#' only those updated on/after `since_date`.
#'
#' @param owner Repository owner
#' @param repo Repository name
#' @param token GitHub personal access token (optional)
#' @param since_date Date lower bound (inclusive)
#' @return List of pull request objects
#' @keywords internal
#' @importFrom gh gh
fetch_pull_requests_since <- function(owner, repo, token, since_date) {
  collected <- list()
  page <- 1L

  repeat {
    page_items <- safe_gh(
      gh::gh,
      "GET /repos/{owner}/{repo}/pulls",
      owner = owner,
      repo = repo,
      state = "all",
      sort = "updated",
      direction = "desc",
      per_page = 100,
      page = page,
      .token = token
    )

    if (is.null(page_items) || !length(page_items)) {
      break
    }

    keep_items <- Filter(function(pr) {
      updated_at <- parse_github_timestamp(pr$updated_at)
      !is.na(updated_at) && updated_at >= since_date
    }, page_items)

    if (length(keep_items)) {
      collected <- c(collected, keep_items)
    }

    if (length(keep_items) < length(page_items)) {
      break
    }

    page <- page + 1L
  }

  collected
}

#' Fetch pull request reviews
#'
#' Internal function to retrieve all review events for a pull request.
#'
#' @param owner Repository owner
#' @param repo Repository name
#' @param pull_number Pull request number
#' @param token GitHub personal access token (optional)
#' @return List of review objects
#' @keywords internal
#' @importFrom gh gh
fetch_pull_request_reviews <- function(owner, repo, pull_number, token) {
  collected <- list()
  page <- 1L

  repeat {
    page_items <- safe_gh(
      gh::gh,
      "GET /repos/{owner}/{repo}/pulls/{pull_number}/reviews",
      owner = owner,
      repo = repo,
      pull_number = pull_number,
      per_page = 100,
      page = page,
      .token = token
    )

    if (is.null(page_items) || !length(page_items)) {
      break
    }

    collected <- c(collected, page_items)
    if (length(page_items) < 100L) {
      break
    }

    page <- page + 1L
  }

  collected
}

#' Parse GitHub timestamp to Date
#'
#' Internal helper to parse ISO 8601 GitHub timestamp fields (e.g.
#' `created_at`, `updated_at`, `submitted_at`) into `Date` values.
#'
#' @param timestamp Character timestamp from GitHub API
#' @return Date value, or NA if unparseable
#' @keywords internal
parse_github_timestamp <- function(timestamp) {
  if (is.null(timestamp) || !length(timestamp)) {
    return(as.Date(NA_character_))
  }

  timestamp <- as.character(timestamp)
  if (!nzchar(timestamp)) {
    return(as.Date(NA_character_))
  }

  parsed <- suppressWarnings(as.POSIXct(timestamp, tz = "UTC", format = "%Y-%m-%dT%H:%M:%OSZ"))
  if (is.na(parsed)) {
    return(as.Date(NA_character_))
  }

  as.Date(parsed)
}

#' Safe GitHub API wrapper
#'
#' Internal function that wraps GitHub API calls to handle 404 errors gracefully.
#' Returns NULL for 404 errors and re-throws other errors.
#'
#' @param fun Function to call (typically gh::gh)
#' @param ... Arguments passed to the function
#' @return Function result or NULL for 404 errors
#' @keywords internal
safe_gh <- function(fun, ...) {
  args <- list(...)
  repo_label <- NULL
  if (!is.null(args$owner) && !is.null(args$repo)) {
    repo_label <- paste0(args$owner, "/", args$repo)
  }

  tryCatch(
    fun(...),
    error = function(err) {
      is_404 <- inherits(err, "http_error_404") ||
        (inherits(err, c("gh_error", "github_error")) && has_status_code(err, 404))

      if (is_404) {
        return(NULL)
      }

      is_403 <- inherits(err, "http_error_403") ||
        (inherits(err, c("gh_error", "github_error")) && has_status_code(err, 403))

      if (is_403) {
        msg <- "GitHub API returned 403 (permission denied)"
        if (!is.null(repo_label)) {
          msg <- paste0(msg, " for ", repo_label)
        }
        msg <- paste0(msg, "; falling back to empty result. For private repos, provide a fine-grained PAT with repository access and read permissions for Contents, Metadata, Issues, and Pull requests.")
        warning(msg, call. = FALSE)
        return(NULL)
      }

      stop(err)
    }
  )
}

#' Check if error has specific HTTP status code
#'
#' Internal function to check if a GitHub API error has a specific status code.
#'
#' @param err Error object from GitHub API
#' @param code HTTP status code to check for
#' @return Logical indicating if error has the specified status code
#' @keywords internal
has_status_code <- function(err, code) {
  status <- tryCatch(err$response$status, error = function(...) NULL)
  if (is.null(status)) {
    status <- tryCatch(err$response_content$status, error = function(...) NULL)
  }
  if (is.null(status)) {
    status <- tryCatch(err$status, error = function(...) NULL)
  }

  if (is.character(status)) {
    status <- suppressWarnings(as.integer(status))
  }

  identical(status, code)
}
