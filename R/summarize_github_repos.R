#' Summarize GitHub repository status
#'
#' `summarize_github_repos()` retrieves release and milestone metadata for a
#' set of GitHub repositories and returns a data frame summarizing their status.
#' @param repos Character vector of repositories in the form `owner/repo`.
#' @param token Optional GitHub personal access token. Defaults to the token
#'   discovered automatically by the gh package when `NULL`.
#' @param qualification_registry Optional data frame of qualification metadata
#'   with columns `org`, `repo`, `version`, `release.url`, `release.date`,
#'   `qualification.url`, and `qualification.date`.
#' @param releases_cache Optional named list of pre-fetched releases, keyed by
#'   `"owner/repo"`. When an entry is present for a repo, the internal
#'   `fetch_releases()` call is skipped.
#'
#' @return A data frame with columns `repo`, `latest_release`, `upcoming_milestones`,
#'   `issue_summary`, `open_prs`, `dev_branch_status`, and `ytd_releases`.
#' @examples
#' \dontrun{
#' summarize_github_repos(c("tidyverse/ggplot2"))
#' }
#' @export
#' @importFrom gh gh
#' @importFrom htmltools htmlEscape
summarize_github_repos <- function(
  repos,
  token = NULL,
  qualification_registry = NULL,
  releases_cache = NULL
) {
  validate_repo_vector(repos)

  registry <- qualification_registry

  results <- vector("list", length(repos))
  repo_snapshot_cache <- new.env(parent = emptyenv())

  for (idx in seq_along(repos)) {
    owner_repo <- repos[[idx]]
    repo_parts <- split_repo_slug(owner_repo)
    owner <- repo_parts$owner
    repo <- repo_parts$repo

    cache_key <- paste(owner, repo, sep = "/")
    if (exists(cache_key, envir = repo_snapshot_cache, inherits = FALSE)) {
      snapshot <- get(cache_key, envir = repo_snapshot_cache, inherits = FALSE)
    } else {
      metadata <- fetch_repo_metadata(owner, repo, token)
      releases <- if (cache_key %in% names(releases_cache)) {
        releases_cache[[cache_key]] %||% list()
      } else {
        fetch_releases(owner, repo, token)
      }
      milestones <- fetch_open_milestones(owner, repo, token)
      pr_count <- fetch_open_prs(owner, repo, token)
      comparison <- fetch_branch_comparison(owner, repo, base = "main", head = "dev", token = token)
      issue_counts  <- fetch_issue_counts(owner, repo, token)

      snapshot <- list(
        metadata = metadata,
        releases = releases,
        milestones = milestones,
        pr_count = pr_count,
        comparison = comparison,
        issues_snapshot = issue_counts$issues_snapshot,
        issues_90day    = issue_counts$issues_90day
      )
      assign(cache_key, snapshot, envir = repo_snapshot_cache)
    }

    is_private <- if (!is.null(snapshot$metadata)) snapshot$metadata$private else FALSE
    releases <- snapshot$releases
    release <- derive_latest_release(releases)
    milestones <- snapshot$milestones
    pr_count <- snapshot$pr_count
    comparison <- snapshot$comparison
    issues_snapshot <- snapshot$issues_snapshot
    issues_90day    <- snapshot$issues_90day

    results[[idx]] <- list(
      repo = format_repo_link(owner, repo, is_private = is_private),
      latest_release = format_release_summary(owner, repo, release, registry),
      upcoming_milestones = format_milestone_summary(owner, repo, milestones),
      issue_summary = format_issue_summary(owner, repo, issues_snapshot, issues_90day),
      open_prs = format_pr_summary(owner, repo, pr_count),
      dev_branch_status = format_branch_comparison(owner, repo, comparison),
      ytd_releases = format_ytd_releases(owner, repo, releases)
    )
  }

  data.frame(
    repo = vapply(results, `[[`, character(1), "repo"),
    latest_release = vapply(results, `[[`, character(1), "latest_release"),
    upcoming_milestones = vapply(results, `[[`, character(1), "upcoming_milestones"),
    issue_summary = vapply(results, `[[`, character(1), "issue_summary"),
    open_prs = vapply(results, `[[`, character(1), "open_prs"),
    dev_branch_status = vapply(results, `[[`, character(1), "dev_branch_status"),
    ytd_releases = vapply(results, `[[`, character(1), "ytd_releases"),
    stringsAsFactors = FALSE
  )
}
