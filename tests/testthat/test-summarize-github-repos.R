# --- summarize_github_repos: releases_cache argument (#39) ---

test_that("summarize_github_repos skips fetch_releases when releases_cache provided #39", {
  pre_fetched_releases <- list(
    "org/repo" = list(
      list(
        tag_name = "v1.0.0",
        html_url = "https://github.com/org/repo/releases/tag/v1.0.0",
        draft = FALSE,
        prerelease = FALSE,
        assets = list(),
        published_at = "2025-01-01T00:00:00Z"
      )
    )
  )
  result <- with_mocked_bindings(
    summarize_github_repos("org/repo", releases_cache = pre_fetched_releases),
    .package = "gh.dash",
    fetch_repo_metadata = function(...) list(private = FALSE, default_branch = "main"),
    fetch_releases = function(...) stop("fetch_releases must not be called when releases_cache provided #39"),
    fetch_open_milestones = function(...) list(),
    fetch_open_prs = function(...) 0L,
    fetch_branch_comparison = function(...) NULL,
    fetch_issue_counts = function(...) list(
      issues_snapshot = list(open = 0L, closed = 0L),
      issues_90day = list(opened = 0L, closed = 0L)
    )
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1L)
})
