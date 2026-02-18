#' Fetch latest release information from GitHub API
#'
#' Internal function to retrieve the latest release for a GitHub repository.
#'
#' @param owner Repository owner (GitHub username or organization)
#' @param repo Repository name
#' @param token GitHub personal access token (optional)
#' @return List with release information or NULL if no release found
#' @keywords internal
#' @importFrom gh gh
fetch_latest_release <- function(owner, repo, token) {
  safe_gh(
    gh::gh,
    "GET /repos/{owner}/{repo}/releases/latest",
    owner = owner,
    repo = repo,
    .token = token
  )
}

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
        msg <- paste0(msg, "; falling back to empty result. Provide a PAT with repo + issues (read) scope for private repos.")
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
