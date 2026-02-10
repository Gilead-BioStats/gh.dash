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
#'
#' @return A data frame with columns `repo`, `latest_release`,
#'   `upcoming_milestones`, `dev_branch_status`, and `ytd_releases`.
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
  qualification_registry = NULL
) {
  validate_repo_vector(repos)

  registry <- qualification_registry

  results <- vector("list", length(repos))

  for (idx in seq_along(repos)) {
    owner_repo <- repos[[idx]]
    pieces <- strsplit(owner_repo, "/", fixed = TRUE)[[1]]
    owner <- pieces[[1]]
    repo <- pieces[[2]]

    release <- fetch_latest_release(owner, repo, token)
    releases <- fetch_releases(owner, repo, token)
    milestones <- fetch_open_milestones(owner, repo, token)
    comparison <- fetch_branch_comparison(owner, repo, base = "main", head = "dev", token = token)

    results[[idx]] <- list(
      repo = format_repo_link(owner, repo),
      latest_release = format_release_summary(owner, repo, release, registry),
      upcoming_milestones = format_milestone_summary(owner, repo, milestones),
      dev_branch_status = format_branch_comparison(owner, repo, comparison),
      ytd_releases = format_ytd_releases(owner, repo, releases)
    )
  }

  data.frame(
    repo = vapply(results, `[[`, character(1), "repo"),
    latest_release = vapply(results, `[[`, character(1), "latest_release"),
    upcoming_milestones = vapply(results, `[[`, character(1), "upcoming_milestones"),
    dev_branch_status = vapply(results, `[[`, character(1), "dev_branch_status"),
    ytd_releases = vapply(results, `[[`, character(1), "ytd_releases"),
    stringsAsFactors = FALSE
  )
}
