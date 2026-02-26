#' Summarize PR activity by user
#'
#' `summarize_pr_activity_by_user()` retrieves pull requests and reviews for a
#' set of repositories and summarizes activity counts by GitHub user.
#'
#' @param repos Character vector of repositories in the form `owner/repo`.
#' @param token Optional GitHub personal access token. Defaults to the token
#'   discovered automatically by the gh package when `NULL`.
#' @param days Number of days to look back from today (inclusive).
#' @param use_cache Logical indicating whether to use local disk cache for
#'   pull request review payloads. Defaults to `TRUE`.
#' @param cache_dir Optional cache directory path. When `NULL`, a platform-
#'   appropriate default cache location is used.
#'
#' @return A data frame with columns `user`, `prs_opened`, `prs_opened_active`,
#'   `prs_reviewed`, `prs_pending_review`, `total_activity`,
#'   `pending_review_repos`, and `opened_active_repos`.
#'   `prs_opened_active` counts PRs the user created within the lookback
#'   window that are still open (not closed or merged).
#'   `opened_active_repos` is a list-column where each element is a character
#'   vector of unique `owner/repo` names contributing to that user's active
#'   opened count.
#'   `prs_pending_review` counts open PRs where the user is currently a
#'   requested reviewer. `pending_review_repos` is a list-column where each
#'   element is a character vector of unique `owner/repo` names contributing
#'   to that user's pending review count. `total_activity` equals
#'   `prs_opened + prs_reviewed` and does not include `prs_pending_review`.
#' @examples
#' \dontrun{
#' summarize_pr_activity_by_user(c("tidyverse/ggplot2"), days = 365)
#' }
#' @export
summarize_pr_activity_by_user <- function(
  repos,
  token = NULL,
  days = 365,
  use_cache = TRUE,
  cache_dir = NULL
) {
  validate_repo_vector(repos)
  days <- validate_lookback_days(days)
  use_cache <- isTRUE(use_cache)
  cache_dir <- resolve_pr_activity_cache_dir(cache_dir)

  since_date <- Sys.Date() - days
  opened_counts <- integer(0)
  opened_active_counts <- integer(0)
  opened_active_repos_list <- list()
  reviewed_counts <- integer(0)
  pending_counts <- integer(0)
  pending_repos_list <- list()

  for (owner_repo in repos) {
    pieces <- strsplit(owner_repo, "/", fixed = TRUE)[[1]]
    owner <- pieces[[1]]
    repo <- pieces[[2]]

    pulls <- fetch_pull_requests_since(owner, repo, token, since_date)
    if (!length(pulls)) {
      next
    }

    for (pr in pulls) {
      author <- first_non_empty(
        as.character(pr$user$login),
        ""
      )
      created_at <- parse_github_timestamp(pr$created_at)

      if (nzchar(author) && !is.na(created_at) && created_at >= since_date) {
        opened_counts <- increment_named_count(opened_counts, author)
      }

      pr_state <- tolower(as.character(pr$state %||% ""))

      if (nzchar(author) && !is.na(created_at) && created_at >= since_date &&
          pr_state == "open") {
        opened_active_counts <- increment_named_count(opened_active_counts, author)
        existing_active <- opened_active_repos_list[[author]]
        if (is.null(existing_active)) {
          opened_active_repos_list[[author]] <- owner_repo
        } else if (!owner_repo %in% existing_active) {
          opened_active_repos_list[[author]] <- c(existing_active, owner_repo)
        }
      }

      if (pr_state == "open" && length(pr$requested_reviewers)) {
        for (reviewer_obj in pr$requested_reviewers) {
          reviewer_login <- as.character(reviewer_obj$login %||% "")
          if (nzchar(reviewer_login)) {
            pending_counts <- increment_named_count(pending_counts, reviewer_login)
            existing <- pending_repos_list[[reviewer_login]]
            if (is.null(existing)) {
              pending_repos_list[[reviewer_login]] <- owner_repo
            } else if (!owner_repo %in% existing) {
              pending_repos_list[[reviewer_login]] <- c(existing, owner_repo)
            }
          }
        }
      }

      number <- suppressWarnings(as.integer(pr$number))
      if (is.na(number)) {
        next
      }

      updated_at <- first_non_empty(as.character(pr$updated_at), "")

      reviews <- fetch_pull_request_reviews_cached(
        owner = owner,
        repo = repo,
        pull_number = number,
        updated_at = updated_at,
        token = token,
        use_cache = use_cache,
        cache_dir = cache_dir
      )
      if (!length(reviews)) {
        next
      }

      reviewers <- unique(vapply(reviews, function(review) {
        submitted_at <- parse_github_timestamp(review$submitted_at)
        login <- first_non_empty(as.character(review$user$login), "")
        state <- toupper(first_non_empty(as.character(review$state), ""))

        if (
          nzchar(login) &&
            !is.na(submitted_at) &&
            submitted_at >= since_date &&
            state != "PENDING"
        ) {
          return(login)
        }

        ""
      }, character(1), USE.NAMES = FALSE))

      reviewers <- reviewers[nzchar(reviewers)]
      if (length(reviewers)) {
        for (reviewer in reviewers) {
          reviewed_counts <- increment_named_count(reviewed_counts, reviewer)
        }
      }
    }
  }

  users <- sort(unique(c(names(opened_counts), names(reviewed_counts), names(pending_counts))))
  if (!length(users)) {
    empty <- data.frame(
      user = character(0),
      prs_opened = integer(0),
      prs_opened_active = integer(0),
      prs_reviewed = integer(0),
      prs_pending_review = integer(0),
      total_activity = integer(0),
      stringsAsFactors = FALSE
    )
    empty$pending_review_repos <- I(list())
    empty$opened_active_repos <- I(list())
    return(empty)
  }

  opened <- as.integer(vapply(users, function(user) {
    value <- unname(opened_counts[user])
    if (!length(value) || is.na(value)) 0L else as.integer(value)
  }, integer(1)))
  reviewed <- as.integer(vapply(users, function(user) {
    value <- unname(reviewed_counts[user])
    if (!length(value) || is.na(value)) 0L else as.integer(value)
  }, integer(1)))
  pending <- as.integer(vapply(users, function(user) {
    value <- unname(pending_counts[user])
    if (!length(value) || is.na(value)) 0L else as.integer(value)
  }, integer(1)))
  opened_active <- as.integer(vapply(users, function(user) {
    value <- unname(opened_active_counts[user])
    if (!length(value) || is.na(value)) 0L else as.integer(value)
  }, integer(1)))
  opened_active_repos_out <- lapply(users, function(user) {
    r <- opened_active_repos_list[[user]]
    if (is.null(r)) character(0) else r
  })
  pending_repos_out <- lapply(users, function(user) {
    r <- pending_repos_list[[user]]
    if (is.null(r)) character(0) else r
  })
  total <- opened + reviewed

  out <- data.frame(
    user = users,
    prs_opened = opened,
    prs_opened_active = opened_active,
    prs_reviewed = reviewed,
    prs_pending_review = pending,
    total_activity = total,
    stringsAsFactors = FALSE
  )
  out$pending_review_repos <- pending_repos_out
  out$opened_active_repos <- opened_active_repos_out

  out[order(-out$total_activity, -out$prs_opened, -out$prs_reviewed, out$user), , drop = FALSE]
}

#' Increment named integer counter
#'
#' Internal helper to increment a named integer vector for a given key.
#'
#' @param counts Named integer vector
#' @param key Character scalar key to increment
#' @return Updated named integer vector
#' @keywords internal
increment_named_count <- function(counts, key) {
  current <- unname(counts[key])
  if (!length(current) || is.na(current)) {
    current <- 0L
  }
  counts[key] <- as.integer(current) + 1L
  counts
}

#' Resolve cache directory for PR activity
#'
#' Internal helper to determine where PR activity cache files are stored.
#'
#' @param cache_dir Optional user-provided cache directory
#' @return Character scalar cache directory path
#' @keywords internal
resolve_pr_activity_cache_dir <- function(cache_dir = NULL) {
  if (!is.null(cache_dir) && nzchar(cache_dir)) {
    return(path.expand(cache_dir))
  }

  if (identical(Sys.info()[["sysname"]], "Darwin")) {
    base <- path.expand("~/Library/Caches")
  } else if (identical(.Platform$OS.type, "windows")) {
    base <- Sys.getenv("LOCALAPPDATA", unset = path.expand("~"))
  } else {
    base <- Sys.getenv("XDG_CACHE_HOME", unset = path.expand("~/.cache"))
  }

  file.path(base, "gh.dash", "pr-review-cache")
}

#' Fetch pull request reviews with local cache
#'
#' Internal helper that reads/writes review payloads on disk keyed by repo,
#' pull number, and PR `updated_at` timestamp to avoid repeat API calls.
#'
#' @param owner Repository owner
#' @param repo Repository name
#' @param pull_number Pull request number
#' @param updated_at Pull request updated timestamp
#' @param token GitHub personal access token
#' @param use_cache Logical indicating whether cache should be used
#' @param cache_dir Directory for cache files
#' @return List of review objects
#' @keywords internal
fetch_pull_request_reviews_cached <- function(
  owner,
  repo,
  pull_number,
  updated_at,
  token,
  use_cache,
  cache_dir
) {
  if (!isTRUE(use_cache) || !nzchar(updated_at)) {
    return(fetch_pull_request_reviews(owner, repo, pull_number, token))
  }

  cache_path <- build_pr_review_cache_path(
    owner = owner,
    repo = repo,
    pull_number = pull_number,
    updated_at = updated_at,
    cache_dir = cache_dir
  )

  cached <- read_pr_review_cache(cache_path)
  if (!is.null(cached)) {
    return(cached)
  }

  reviews <- fetch_pull_request_reviews(owner, repo, pull_number, token)
  write_pr_review_cache(cache_path, reviews)
  reviews
}

#' Build cache path for pull request reviews
#'
#' @param owner Repository owner
#' @param repo Repository name
#' @param pull_number Pull request number
#' @param updated_at Pull request updated timestamp
#' @param cache_dir Directory for cache files
#' @return Character scalar cache file path
#' @keywords internal
build_pr_review_cache_path <- function(owner, repo, pull_number, updated_at, cache_dir) {
  safe_updated <- sanitize_cache_component(updated_at)
  file.path(
    cache_dir,
    sanitize_cache_component(owner),
    sanitize_cache_component(repo),
    sprintf("pr-%s-%s.rds", as.integer(pull_number), safe_updated)
  )
}

#' Sanitize cache path component
#'
#' @param x Character value to sanitize for file paths
#' @return Character scalar with safe path characters
#' @keywords internal
sanitize_cache_component <- function(x) {
  value <- first_non_empty(as.character(x), "unknown")
  gsub("[^A-Za-z0-9._-]", "_", value)
}

#' Read pull request review cache
#'
#' @param path Cache file path
#' @return Cached review list or NULL
#' @keywords internal
read_pr_review_cache <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }

  tryCatch(readRDS(path), error = function(...) NULL)
}

#' Write pull request review cache
#'
#' @param path Cache file path
#' @param reviews Review list payload
#' @return Invisibly TRUE
#' @keywords internal
write_pr_review_cache <- function(path, reviews) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tryCatch(saveRDS(reviews, path), error = function(...) NULL)
  invisible(TRUE)
}
