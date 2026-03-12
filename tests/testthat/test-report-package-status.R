test_that("summarize_github_repos validates repository format", {
  expect_error(summarize_github_repos(42), "character vector")
  expect_error(summarize_github_repos(character(0)), "at least one")
  expect_error(summarize_github_repos(c("")), "cannot contain empty strings")
  expect_error(
    summarize_github_repos(c("invalid")),
    "owner/repo"
  )
})

# -- YTD release helpers -------------------------------------------------------

test_that("parse_release_date returns Date for valid ISO 8601 input", {
  expect_equal(gh.dash:::parse_release_date("2026-03-15T10:00:00Z"), as.Date("2026-03-15"))
})

test_that("parse_release_date returns NA for NULL or empty input", {
  expect_true(is.na(gh.dash:::parse_release_date(NULL)))
  expect_true(is.na(gh.dash:::parse_release_date("")))
})

test_that("count_ytd_releases counts only current-year non-draft non-prerelease", {
  current_year <- format(Sys.Date(), "%Y")
  releases <- list(
    list(published_at = paste0(current_year, "-01-05T00:00:00Z"), draft = FALSE, prerelease = FALSE),
    list(published_at = paste0(current_year, "-02-01T00:00:00Z"), draft = FALSE, prerelease = FALSE),
    list(published_at = paste0(current_year, "-01-10T00:00:00Z"), draft = TRUE, prerelease = FALSE),
    list(published_at = paste0(current_year, "-01-20T00:00:00Z"), draft = FALSE, prerelease = TRUE),
    list(published_at = "2020-06-01T00:00:00Z", draft = FALSE, prerelease = FALSE),
    list(published_at = NULL, draft = FALSE, prerelease = FALSE)
  )
  expect_equal(gh.dash:::count_ytd_releases(releases), 2L)
})

test_that("count_ytd_releases returns 0 for NULL or empty list", {
  expect_equal(gh.dash:::count_ytd_releases(NULL), 0L)
  expect_equal(gh.dash:::count_ytd_releases(list()), 0L)
})

test_that("format_ytd_releases always returns a hyperlink", {
  result <- gh.dash:::format_ytd_releases("org", "repo", NULL)
  expect_match(result, "<a href=\"https://github.com/org/repo/releases\" title=\"0 releases in past 90 days\">0</a>")
})

test_that("derive_latest_release skips drafts and prereleases", {
  releases <- list(
    list(tag_name = "v2.0.0-rc1", draft = FALSE, prerelease = TRUE),
    list(tag_name = "v1.1.0-draft", draft = TRUE, prerelease = FALSE),
    list(tag_name = "v1.0.0", draft = FALSE, prerelease = FALSE)
  )
  latest <- gh.dash:::derive_latest_release(releases)
  expect_equal(latest$tag_name, "v1.0.0")
})

test_that("derive_latest_release returns NULL when all are drafts or prereleases", {
  releases <- list(
    list(tag_name = "v2.0.0-rc1", draft = FALSE, prerelease = TRUE),
    list(tag_name = "v1.1.0-draft", draft = TRUE, prerelease = FALSE)
  )
  expect_null(gh.dash:::derive_latest_release(releases))
  expect_null(gh.dash:::derive_latest_release(NULL))
  expect_null(gh.dash:::derive_latest_release(list()))
})

test_that("summarize_github_repos assembles release and milestone summaries", {
  mock_releases <- list(
    list(
      tag_name = "v1.0.0",
      published_at = "2025-01-01T12:00:00Z",
      html_url = "https://github.com/org/repo/releases/tag/v1.0.0",
      draft = FALSE,
      prerelease = FALSE
    )
  )
  mock_milestones <- list(
    list(
      title = "Milestone A",
      open_issues = 3,
      closed_issues = 2,
      html_url = "https://github.com/org/repo/milestone/1"
    ),
    list(title = "Backlog", open_issues = 0, closed_issues = 0)
  )
  mock_comparison <- list(ahead_by = 2, behind_by = 1)

  result <- with_mocked_bindings(
    summarize_github_repos("org/repo"),
    fetch_repo_metadata = function(...) list(private = FALSE),
    fetch_releases = function(owner, repo, token) {
      expect_equal(owner, "org")
      expect_equal(repo, "repo")
      mock_releases
    },
    fetch_open_milestones = function(owner, repo, token) {
      expect_equal(owner, "org")
      expect_equal(repo, "repo")
      mock_milestones
    },
    fetch_open_prs = function(...) 0L,
    fetch_issue_counts = function(...) list(issues_snapshot = list(open = 0L, closed = 0L), issues_90day = list(opened = 0L, closed = 0L)),
    fetch_branch_comparison = function(owner, repo, base, head, token) {
      expect_equal(
        list(owner = owner, repo = repo, base = base, head = head),
        list(owner = "org", repo = "repo", base = "main", head = "dev")
      )
      mock_comparison
    }
  )

  expect_s3_class(result, "data.frame")
  expect_equal(result$repo, "<a href=\"https://github.com/org/repo\">org/repo</a>")
  expect_match(result$latest_release, "<a href=\"https://github.com/org/repo/releases/tag/v1.0.0\">")
  expect_match(result$latest_release, "Released 2025-01-01")
  expect_match(result$latest_release, "span")
  expect_equal(
    result$dev_branch_status,
    "<a href=\"https://github.com/org/repo/compare/main...dev\">+2, -1</a>"
  )
  expect_match(result$upcoming_milestones, "<a href=\"https://github.com/org/repo/milestone/1\">")
  expect_match(result$upcoming_milestones, "3 open of 5")
  expect_match(result$ytd_releases, "<a href=\"https://github.com/org/repo/releases\" title=\"0 releases in past 90 days\">")
})

test_that("summarize_github_repos includes open PR count", {
  result <- with_mocked_bindings(
    summarize_github_repos("org/repo"),
    fetch_repo_metadata = function(owner, repo, token) {
      expect_equal(owner, "org")
      expect_equal(repo, "repo")
      list(private = FALSE)
    },
    fetch_releases = function(...) list(),
    fetch_open_milestones = function(...) list(),
    fetch_open_prs = function(owner, repo, token) {
      expect_equal(owner, "org")
      expect_equal(repo, "repo")
      3L
    },
    fetch_issue_counts = function(...) list(issues_snapshot = list(open = 0L, closed = 0L), issues_90day = list(opened = 0L, closed = 0L)),
    fetch_branch_comparison = function(...) list(ahead_by = 0, behind_by = 0)
  )

  expect_equal(result$open_prs, "<a href=\"https://github.com/org/repo/pulls\">3</a>")
})

test_that("summarize_github_repos appends qualification badge when registry matches", {
  mock_release <- list(
    tag_name = "v1.0.0",
    published_at = "2025-01-01T12:00:00Z",
    html_url = "https://github.com/org/repo/releases/tag/v1.0.0",
    draft = FALSE,
    prerelease = FALSE
  )

  registry <- data.frame(
    org = "org",
    repo = "repo",
    version = "v1.0.0",
    release.url = "https://github.com/org/repo/releases/tag/v1.0.0",
    release.date = "2025-01-01",
    qualification.url = "https://github.com/Gilead-BioStats/r-qualification/blob/main/org/qualification_v1_0_0.md",
    qualification.date = "2025-01-15",
    stringsAsFactors = FALSE
  )

  result <- with_mocked_bindings(
    summarize_github_repos("org/repo", qualification_registry = registry),
    fetch_repo_metadata = function(...) list(private = FALSE),
    fetch_releases = function(...) list(mock_release),
    fetch_open_milestones = function(...) list(),
    fetch_open_prs = function(...) 0L,
    fetch_issue_counts = function(...) list(issues_snapshot = list(open = 0L, closed = 0L), issues_90day = list(opened = 0L, closed = 0L)),
    fetch_branch_comparison = function(...) list(ahead_by = 0, behind_by = 0)
  )

  expect_match(result$latest_release, "128737")
  expect_match(result$latest_release, "qualification_v1_0_0")
})

test_that("summarize_github_repos shows grey badge when older version qualified", {
  mock_release <- list(
    tag_name = "v1.1.0",
    published_at = "2025-02-01T12:00:00Z",
    html_url = "https://github.com/org/repo/releases/tag/v1.1.0",
    draft = FALSE,
    prerelease = FALSE
  )

  registry <- data.frame(
    org = "org",
    repo = "repo",
    version = "v1.0.0",
    release.url = "https://github.com/org/repo/releases/tag/v1.0.0",
    release.date = "2025-01-01",
    qualification.url = "https://github.com/Gilead-BioStats/r-qualification/blob/main/org/qualification_v1_0_0.md",
    qualification.date = "2025-01-15",
    stringsAsFactors = FALSE
  )

  result <- with_mocked_bindings(
    summarize_github_repos("org/repo", qualification_registry = registry),
    fetch_repo_metadata = function(...) list(private = FALSE),
    fetch_releases = function(...) list(mock_release),
    fetch_open_milestones = function(...) list(),
    fetch_open_prs = function(...) 0L,
    fetch_issue_counts = function(...) list(issues_snapshot = list(open = 0L, closed = 0L), issues_90day = list(opened = 0L, closed = 0L)),
    fetch_branch_comparison = function(...) list(ahead_by = 0, behind_by = 0)
  )

  expect_match(result$latest_release, "badge--slate")
  expect_match(result$latest_release, "128737")
  expect_match(result$latest_release, "qualification_v1_0_0")
})

test_that("summarize_github_repos supports multiple repositories", {
  all_releases <- list(
    list(
      list(
        tag_name = "v1.1.0",
        published_at = "2025-02-15T00:00:00Z",
        html_url = "https://github.com/org/repo/releases/tag/v1.1.0",
        draft = FALSE,
        prerelease = FALSE
      )
    ),
    list()
  )
  milestones <- list(
    list(list(
      title = "Milestone B",
      open_issues = 1,
      closed_issues = 4,
      html_url = "https://github.com/org/repo/milestone/2"
    )),
    list()
  )
  comparisons <- list(
    list(ahead_by = 0, behind_by = 0),
    list(ahead_by = 0, behind_by = 3)
  )

  index <- 0
  result <- with_mocked_bindings(
    summarize_github_repos(c("org/repo", "org2/repo2")),
    fetch_repo_metadata = function(...) list(private = FALSE),
    fetch_releases = function(owner, repo, token) {
      index <<- index + 1
      all_releases[[index]]
    },
    fetch_open_milestones = function(owner, repo, token) {
      milestones[[index]]
    },
    fetch_open_prs = function(...) 0L,
    fetch_issue_counts = function(...) list(issues_snapshot = list(open = 0L, closed = 0L), issues_90day = list(opened = 0L, closed = 0L)),
    fetch_branch_comparison = function(owner, repo, base, head, token) {
      comparisons[[index]]
    }
  )

  expect_equal(nrow(result), 2)
  expect_equal(result$repo[[1]], "<a href=\"https://github.com/org/repo\">org/repo</a>")
  expect_equal(result$repo[[2]], "<a href=\"https://github.com/org2/repo2\">org2/repo2</a>")
  expect_match(result$latest_release[[1]], "<a href=\"https://github.com/org/repo/releases/tag/v1.1.0\">")
  expect_match(result$latest_release[[1]], "2025-02-15")
  expect_match(result$latest_release[[2]], "<a href=\"https://github.com/org2/repo2/releases\">")
  expect_match(result$latest_release[[2]], "No release")
  expect_equal(
    result$dev_branch_status[[1]],
    "<a href=\"https://github.com/org/repo/compare/main...dev\">In sync</a>"
  )
  expect_equal(
    result$dev_branch_status[[2]],
    "<a href=\"https://github.com/org2/repo2/compare/main...dev\">-3</a>"
  )
  expect_match(result$upcoming_milestones[[1]], "<a href=\"https://github.com/org/repo/milestone/2\">")
  expect_match(result$upcoming_milestones[[1]], "1 open of 5")
  expect_match(result$upcoming_milestones[[2]], "<a href=\"https://github.com/org2/repo2/milestones\">")
  expect_match(result$upcoming_milestones[[2]], "None")
  expect_match(result$ytd_releases[[1]], "<a href=\"https://github.com/org/repo/releases\" title=\"0 releases in past 90 days\">")
  expect_match(result$ytd_releases[[2]], "<a href=\"https://github.com/org2/repo2/releases\" title=\"0 releases in past 90 days\">")
  expect_match(result$ytd_releases[[2]], ">0</a>")
})

test_that("summarize_github_repos reuses API payloads for duplicate repos", {
  calls <- new.env(parent = emptyenv())
  calls$metadata <- 0L
  calls$releases <- 0L
  calls$milestones <- 0L
  calls$prs <- 0L
  calls$issue_counts <- 0L
  calls$comparison <- 0L

  result <- with_mocked_bindings(
    summarize_github_repos(c("org/repo", "org/repo")),
    fetch_repo_metadata = function(...) {
      calls$metadata <- calls$metadata + 1L
      list(private = FALSE)
    },
    fetch_releases = function(...) {
      calls$releases <- calls$releases + 1L
      list(list(
        tag_name = "v1.0.0",
        published_at = "2025-01-01T12:00:00Z",
        html_url = "https://github.com/org/repo/releases/tag/v1.0.0",
        draft = FALSE,
        prerelease = FALSE
      ))
    },
    fetch_open_milestones = function(...) {
      calls$milestones <- calls$milestones + 1L
      list()
    },
    fetch_open_prs = function(...) {
      calls$prs <- calls$prs + 1L
      0L
    },
    fetch_issue_counts = function(...) {
      calls$issue_counts <- calls$issue_counts + 1L
      list(
        issues_snapshot = list(open = 0L, closed = 0L),
        issues_90day    = list(opened = 0L, closed = 0L)
      )
    },
    fetch_branch_comparison = function(...) {
      calls$comparison <- calls$comparison + 1L
      list(ahead_by = 0, behind_by = 0)
    }
  )

  expect_equal(calls$metadata, 1L)
  expect_equal(calls$releases, 1L)
  expect_equal(calls$milestones, 1L)
  expect_equal(calls$prs, 1L)
  expect_equal(calls$issue_counts, 1L)
  expect_equal(calls$comparison, 1L)
  expect_equal(nrow(result), 2L)
  expect_equal(result$repo[[1]], result$repo[[2]])
  expect_equal(result$latest_release[[1]], result$latest_release[[2]])
})

test_that("summarize_pr_activity_by_user validates lookback days", {
  expect_error(summarize_pr_activity_by_user("org/repo", days = 0, use_cache = FALSE), "positive integer")
  expect_error(summarize_pr_activity_by_user("org/repo", days = NA, use_cache = FALSE), "single non-missing")
})

test_that("summarize_pr_activity_by_user aggregates opened and reviewed PRs by user", {
  now <- as.POSIXct(Sys.time(), tz = "UTC")
  ts <- function(days_ago) format(now - days_ago * 86400, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  pulls <- list(
    list(
      number = 10,
      state = "open",
      created_at = ts(10),
      updated_at = ts(8),
      user = list(login = "alice"),
      requested_reviewers = list(list(login = "carol"))
    ),
    list(
      number = 11,
      state = "closed",
      created_at = ts(9),
      updated_at = ts(8),
      user = list(login = "bob"),
      requested_reviewers = list(list(login = "alice"))
    )
  )

  result <- with_mocked_bindings(
    summarize_pr_activity_by_user("org/repo", days = 365, use_cache = FALSE),
    fetch_pull_requests_since = function(owner, repo, token, since_date) {
      expect_equal(owner, "org")
      expect_equal(repo, "repo")
      pulls
    },
    fetch_pull_request_reviews = function(owner, repo, pull_number, token) {
      if (pull_number == 10) {
        return(list(
          list(submitted_at = ts(7), state = "APPROVED", user = list(login = "bob")),
          list(submitted_at = ts(7), state = "COMMENTED", user = list(login = "bob"))
        ))
      }

      list(
        list(submitted_at = ts(6), state = "CHANGES_REQUESTED", user = list(login = "alice")),
        list(submitted_at = ts(6), state = "APPROVED", user = list(login = "carol"))
      )
    }
  )

  expect_s3_class(result, "data.frame")
  expect_equal(sort(result$user), c("alice", "bob", "carol"))

  alice <- result[result$user == "alice", , drop = FALSE]
  bob <- result[result$user == "bob", , drop = FALSE]
  carol <- result[result$user == "carol", , drop = FALSE]

  expect_equal(alice$prs_opened, 1L)
  expect_equal(alice$prs_opened_active, 1L)
  expect_equal(alice$opened_active_repos[[1]], "org/repo")
  expect_equal(alice$prs_reviewed, 1L)
  expect_equal(alice$total_activity, 2L)
  expect_equal(alice$prs_pending_review, 0L)

  expect_equal(bob$prs_opened, 1L)
  expect_equal(bob$prs_opened_active, 0L)
  expect_equal(length(bob$opened_active_repos[[1]]), 0L)
  expect_equal(bob$prs_reviewed, 1L)
  expect_equal(bob$total_activity, 2L)
  expect_equal(bob$prs_pending_review, 0L)

  expect_equal(carol$prs_opened, 0L)
  expect_equal(carol$prs_opened_active, 0L)
  expect_equal(carol$prs_reviewed, 1L)
  expect_equal(carol$total_activity, 1L)
  expect_equal(carol$prs_pending_review, 1L)
  expect_equal(carol$pending_review_repos[[1]], "org/repo")
})

test_that("summarize_pr_activity_by_user returns empty data frame when no activity", {
  result <- with_mocked_bindings(
    summarize_pr_activity_by_user("org/repo", days = 365, use_cache = FALSE),
    fetch_pull_requests_since = function(...) list()
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("user", "prs_opened", "prs_opened_active", "prs_reviewed", "prs_pending_review", "total_activity", "pending_review_repos", "opened_active_repos"))
})

test_that("summarize_pr_activity_by_user counts pending reviews and deduplicates repos per user", {
  now <- as.POSIXct(Sys.time(), tz = "UTC")
  ts <- function(days_ago) format(now - days_ago * 86400, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  pr20_created <- ts(7)
  pr20_updated <- ts(6)
  pr21_created <- ts(5)
  pr21_updated <- ts(4)
  pr22_created <- ts(3)
  pr22_updated <- ts(2)

  pulls <- list(
    list(
      number = 20,
      state = "open",
      created_at = pr20_created,
      updated_at = pr20_updated,
      user = list(login = "alice"),
      requested_reviewers = list(list(login = "bob"), list(login = "carol"))
    ),
    list(
      number = 21,
      state = "open",
      created_at = pr21_created,
      updated_at = pr21_updated,
      user = list(login = "alice"),
      requested_reviewers = list(list(login = "bob"))
    ),
    list(
      number = 22,
      state = "closed",
      created_at = pr22_created,
      updated_at = pr22_updated,
      user = list(login = "alice"),
      requested_reviewers = list(list(login = "carol"))
    )
  )

  result <- with_mocked_bindings(
    summarize_pr_activity_by_user("org/repo", days = 365, use_cache = FALSE),
    fetch_pull_requests_since = function(owner, repo, token, since_date) pulls,
    fetch_pull_request_reviews = function(...) list()
  )

  bob <- result[result$user == "bob", , drop = FALSE]
  carol <- result[result$user == "carol", , drop = FALSE]

  # bob is requested on PR #20 and #21 (both open) — count = 2, one unique repo
  expect_equal(bob$prs_pending_review, 2L)
  expect_equal(bob$pending_review_repos[[1]], "org/repo")
  expect_equal(length(bob$pending_review_repos[[1]]), 1L)

  # carol is requested on PR #20 (open) and PR #22 (closed — ignored) — count = 1
  expect_equal(carol$prs_pending_review, 1L)
  expect_equal(carol$pending_review_repos[[1]], "org/repo")

  # alice opened all 3 PRs; only #20 and #21 are open => active = 2
  alice <- result[result$user == "alice", , drop = FALSE]
  expect_equal(alice$prs_opened, 3L)
  expect_equal(alice$prs_opened_active, 2L)
  expect_equal(alice$opened_active_repos[[1]], "org/repo")
})

test_that("summarize_pr_activity_by_user reuses cached review payloads", {
  now <- as.POSIXct(Sys.time(), tz = "UTC")
  ts <- function(days_ago) format(now - days_ago * 86400, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  pulls <- list(
    list(
      number = 42,
      created_at = ts(10),
      updated_at = ts(8),
      user = list(login = "alice")
    )
  )

  review_calls <- 0L
  cache_dir <- file.path(tempdir(), paste0("ghdash-cache-", as.integer(Sys.time())))

  first <- with_mocked_bindings(
    summarize_pr_activity_by_user(
      "org/repo",
      days = 365,
      use_cache = TRUE,
      cache_dir = cache_dir
    ),
    fetch_pull_requests_since = function(...) pulls,
    fetch_pull_request_reviews = function(...) {
      review_calls <<- review_calls + 1L
      list(list(
        submitted_at = ts(7),
        state = "APPROVED",
        user = list(login = "bob")
      ))
    }
  )

  second <- with_mocked_bindings(
    summarize_pr_activity_by_user(
      "org/repo",
      days = 365,
      use_cache = TRUE,
      cache_dir = cache_dir
    ),
    fetch_pull_requests_since = function(...) pulls,
    fetch_pull_request_reviews = function(...) {
      review_calls <<- review_calls + 1L
      list(list(
        submitted_at = ts(7),
        state = "APPROVED",
        user = list(login = "bob")
      ))
    }
  )

  expect_equal(review_calls, 1L)
  expect_equal(first, second)
})

test_that("summarize_pr_activity_by_user reuses API payloads for duplicate repos", {
  now <- as.POSIXct(Sys.time(), tz = "UTC")
  ts <- function(days_ago) format(now - days_ago * 86400, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  pulls <- list(
    list(
      number = 42,
      created_at = ts(10),
      updated_at = ts(8),
      user = list(login = "alice")
    )
  )

  pull_calls <- 0L
  review_calls <- 0L

  result <- with_mocked_bindings(
    summarize_pr_activity_by_user(c("org/repo", "org/repo"), days = 365, use_cache = FALSE),
    fetch_pull_requests_since = function(...) {
      pull_calls <<- pull_calls + 1L
      pulls
    },
    fetch_pull_request_reviews = function(...) {
      review_calls <<- review_calls + 1L
      list(list(
        submitted_at = ts(7),
        state = "APPROVED",
        user = list(login = "bob")
      ))
    }
  )

  expect_equal(pull_calls, 1L)
  expect_equal(review_calls, 1L)

  alice <- result[result$user == "alice", , drop = FALSE]
  bob <- result[result$user == "bob", , drop = FALSE]

  expect_equal(alice$prs_opened, 2L)
  expect_equal(alice$total_activity, 2L)
  expect_equal(bob$prs_reviewed, 2L)
  expect_equal(bob$total_activity, 2L)
})

test_that("summarize_pr_activity_by_user handles PRs with no reviews and no requested reviewers", {
  # Exercises the initialized-but-empty reviewed_counts / pending_counts path
  now <- as.POSIXct(Sys.time(), tz = "UTC")
  ts <- function(days_ago) format(now - days_ago * 86400, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  pulls <- list(
    list(
      number = 5,
      state = "open",
      created_at = ts(10),
      updated_at = ts(8),
      user = list(login = "alice"),
      requested_reviewers = list()
    )
  )

  result <- with_mocked_bindings(
    summarize_pr_activity_by_user("org/repo", days = 365, use_cache = FALSE),
    fetch_pull_requests_since = function(...) pulls,
    fetch_pull_request_reviews = function(...) list()
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1L)
  alice <- result[result$user == "alice", , drop = FALSE]
  expect_equal(alice$prs_opened, 1L)
  expect_equal(alice$prs_reviewed, 0L)
  expect_equal(alice$prs_pending_review, 0L)
  expect_equal(alice$total_activity, 1L)
})

test_that("summarize_pr_activity_by_user excludes PENDING review state", {
  now <- as.POSIXct(Sys.time(), tz = "UTC")
  ts <- function(days_ago) format(now - days_ago * 86400, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  pulls <- list(
    list(
      number = 7,
      state = "open",
      created_at = ts(10),
      updated_at = ts(8),
      user = list(login = "alice"),
      requested_reviewers = list()
    )
  )

  result <- with_mocked_bindings(
    summarize_pr_activity_by_user("org/repo", days = 365, use_cache = FALSE),
    fetch_pull_requests_since = function(...) pulls,
    fetch_pull_request_reviews = function(...) {
      list(
        list(submitted_at = ts(7), state = "PENDING", user = list(login = "bob"))
      )
    }
  )

  # "bob" has only a PENDING review — must not appear in reviewed_counts
  expect_false("bob" %in% result$user)
  alice <- result[result$user == "alice", , drop = FALSE]
  expect_equal(alice$prs_reviewed, 0L)
})

test_that("summarize_pr_activity_by_user excludes reviews outside the lookback window", {
  now <- as.POSIXct(Sys.time(), tz = "UTC")
  ts <- function(days_ago) format(now - days_ago * 86400, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  pulls <- list(
    list(
      number = 8,
      state = "open",
      created_at = ts(5),
      updated_at = ts(3),
      user = list(login = "alice"),
      requested_reviewers = list()
    )
  )

  result <- with_mocked_bindings(
    # Use 30-day window; review was submitted 60 days ago
    summarize_pr_activity_by_user("org/repo", days = 30, use_cache = FALSE),
    fetch_pull_requests_since = function(...) pulls,
    fetch_pull_request_reviews = function(...) {
      list(
        list(
          submitted_at = ts(60),
          state = "APPROVED",
          user = list(login = "bob")
        )
      )
    }
  )

  # "bob"'s review is outside the window — must not be counted
  expect_false("bob" %in% result$user)
})

test_that("summarize_pr_activity_by_user skips review fetch for PR with non-numeric number", {
  now <- as.POSIXct(Sys.time(), tz = "UTC")
  ts <- function(days_ago) format(now - days_ago * 86400, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  pulls <- list(
    list(
      number = "not-a-number",
      state = "open",
      created_at = ts(10),
      updated_at = ts(8),
      user = list(login = "alice"),
      requested_reviewers = list()
    )
  )

  review_calls <- 0L

  result <- with_mocked_bindings(
    summarize_pr_activity_by_user("org/repo", days = 365, use_cache = FALSE),
    fetch_pull_requests_since = function(...) pulls,
    fetch_pull_request_reviews = function(...) {
      review_calls <<- review_calls + 1L
      list()
    }
  )

  # Review fetch must be skipped entirely for bad PR numbers
  expect_equal(review_calls, 0L)
  alice <- result[result$user == "alice", , drop = FALSE]
  expect_equal(alice$prs_opened, 1L)
  expect_equal(alice$prs_reviewed, 0L)
})

test_that("summarize_github_repos renders lock icon for private repos", {
  result <- with_mocked_bindings(
    summarize_github_repos("org/repo"),
    fetch_repo_metadata = function(...) list(private = TRUE),
    fetch_releases = function(...) list(),
    fetch_open_milestones = function(...) list(),
    fetch_open_prs = function(...) 0L,
    fetch_issue_counts = function(...) list(issues_snapshot = list(open = 0L, closed = 0L), issues_90day = list(opened = 0L, closed = 0L)),
    fetch_branch_comparison = function(...) list(ahead_by = 0, behind_by = 0)
  )

  expect_match(result$repo, "\U0001F512")
  expect_match(result$repo, "Private repository")
  expect_match(result$repo, "org/repo")
})

test_that("summarize_github_repos defaults is_private to FALSE when metadata is NULL", {
  result <- with_mocked_bindings(
    summarize_github_repos("org/repo"),
    fetch_repo_metadata = function(...) NULL,
    fetch_releases = function(...) list(),
    fetch_open_milestones = function(...) list(),
    fetch_open_prs = function(...) 0L,
    fetch_issue_counts = function(...) list(issues_snapshot = list(open = 0L, closed = 0L), issues_90day = list(opened = 0L, closed = 0L)),
    fetch_branch_comparison = function(...) list(ahead_by = 0, behind_by = 0)
  )

  # No lock icon — should render as a plain public link
  expect_false(grepl("\U0001F512", result$repo))
  expect_match(result$repo, "org/repo")
})

# -- format_issue_summary ------------------------------------------------------

test_that("format_issue_summary returns link to issues page", {
  result <- gh.dash:::format_issue_summary("org", "repo", list(open = 5L, closed = 12L), list(opened = 3L, closed = 7L))
  expect_match(result, 'href="https://github.com/org/repo/issues"')
})

test_that("format_issue_summary tooltip shows 90-day opened and closed counts", {
  result <- gh.dash:::format_issue_summary("org", "repo", list(open = 5L, closed = 12L), list(opened = 3L, closed = 7L))
  expect_match(result, "3 opened / 7 closed in past 90 days")
})

test_that("format_issue_summary shows total open count / total closed count as link text", {
  result <- gh.dash:::format_issue_summary("org", "repo", list(open = 5L, closed = 12L), list(opened = 3L, closed = 7L))
  expect_match(result, ">5 / 12<")
})

test_that("format_issue_summary shows 0 / 0 when all counts are zero", {
  result <- gh.dash:::format_issue_summary("org", "repo", list(open = 0L, closed = 0L), list(opened = 0L, closed = 0L))
  expect_match(result, ">0 / 0<")
})

test_that("format_issue_summary shows Unavailable with generic message when reason is absent", {
  result <- gh.dash:::format_issue_summary(
    "org", "repo",
    list(open = NA_integer_, closed = NA_integer_),
    list(opened = NA_integer_, closed = NA_integer_)
  )
  expect_match(result, ">Unavailable<")
  expect_match(result, "Issue data unavailable")
  expect_no_match(result, "rate limit")
})

test_that("format_issue_summary shows rate-limit message when reason is rate_limited", {
  result <- gh.dash:::format_issue_summary(
    "org", "repo",
    list(open = NA_integer_, closed = NA_integer_, reason = "rate_limited"),
    list(opened = NA_integer_, closed = NA_integer_, reason = "rate_limited")
  )
  expect_match(result, ">Unavailable<")
  expect_match(result, "rate limit reached")
})

test_that("summarize_github_repos includes issue_summary column", {
  result <- with_mocked_bindings(
    summarize_github_repos("org/repo"),
    fetch_repo_metadata = function(...) list(private = FALSE),
    fetch_releases = function(...) list(),
    fetch_open_milestones = function(...) list(),
    fetch_open_prs = function(...) 0L,
    fetch_issue_counts = function(...) list(
      issues_snapshot = list(open = 4L, closed = 17L),
      issues_90day    = list(opened = 2L, closed = 5L)
    ),
    fetch_branch_comparison = function(...) list(ahead_by = 0, behind_by = 0)
  )

  expect_true("issue_summary" %in% names(result))
  expect_match(result$issue_summary, 'href="https://github.com/org/repo/issues"')
  expect_match(result$issue_summary, ">4 / 17<")
  expect_match(result$issue_summary, "2 opened / 5 closed in past 90 days")
})
