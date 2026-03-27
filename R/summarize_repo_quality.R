#' Fetch repository git tree from GitHub API
#'
#' Internal function to retrieve the full repository tree for a ref.
#'
#' @param owner Repository owner (GitHub username or organization)
#' @param repo Repository name
#' @param ref Git ref (branch name, tag, or SHA)
#' @param token GitHub personal access token (optional)
#' @return List response from GitHub tree API, or NULL on failure
#' @keywords internal
#' @importFrom gh gh
fetch_repo_git_tree <- function(owner, repo, ref, token) {
  safe_gh(
    gh::gh,
    "GET /repos/{owner}/{repo}/git/trees/{ref}",
    owner = owner,
    repo = repo,
    ref = ref,
    recursive = 1,
    .token = token
  )
}

#' Fetch file content from GitHub API
#'
#' Internal function to retrieve file contents for a specific path/ref.
#'
#' @param owner Repository owner (GitHub username or organization)
#' @param repo Repository name
#' @param path Repository-relative file path
#' @param ref Git ref (branch name, tag, or SHA)
#' @param token GitHub personal access token (optional)
#' @return List response from GitHub contents API, or NULL on failure
#' @keywords internal
#' @importFrom gh gh
fetch_repo_file_content <- function(owner, repo, path, ref, token) {
  safe_gh(
    gh::gh,
    "GET /repos/{owner}/{repo}/contents/{path}",
    owner = owner,
    repo = repo,
    path = path,
    ref = ref,
    .token = token
  )
}

#' Count test_that calls in decoded R source
#'
#' @param source_text Character string with R code
#' @return Integer number of `test_that(` occurrences
#' @keywords internal
count_test_that_calls <- function(source_text) {
  if (is.null(source_text) || !length(source_text)) {
    return(0L)
  }

  source_text <- as.character(source_text)
  if (!nzchar(source_text)) {
    return(0L)
  }

  matches <- gregexpr("test_that\\s*\\(", source_text, perl = TRUE)[[1]]
  if (length(matches) == 1L && matches[[1]] == -1L) {
    return(0L)
  }

  as.integer(length(matches))
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
summarize_repo_quality <- function(repos, token = NULL) {
  validate_repo_vector(repos)

  results <- vector("list", length(repos))

  for (idx in seq_along(repos)) {
    owner_repo <- repos[[idx]]
    repo_parts <- split_repo_slug(owner_repo)
    owner <- repo_parts$owner
    repo <- repo_parts$repo

    metadata <- fetch_repo_metadata(owner, repo, token)
    default_branch <- metadata$default_branch %||% "HEAD"
    is_private <- metadata$private %||% FALSE

    tree <- fetch_repo_git_tree(owner, repo, default_branch, token)
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
        test_total <- 0L
        for (path in test_file_paths) {
          content <- fetch_repo_file_content(owner, repo, path, default_branch, token)
          encoded <- content$content
          if (is.null(encoded) || !length(encoded)) {
            next
          }

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
