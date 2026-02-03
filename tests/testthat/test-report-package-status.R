test_that("summarize_github_repos validates repository format", {
  expect_error(summarize_github_repos(42), "character vector")
  expect_error(summarize_github_repos(character(0)), "at least one")
  expect_error(summarize_github_repos(c("")), "cannot contain empty strings")
  expect_error(
    summarize_github_repos(c("invalid")),
    "owner/repo"
  )
})

test_that("summarize_github_repos assembles release and milestone summaries", {
  mock_release <- list(
    tag_name = "v1.0.0",
    published_at = "2025-01-01T12:00:00Z",
    html_url = "https://github.com/org/repo/releases/tag/v1.0.0"
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
    fetch_latest_release = function(owner, repo, token) {
      expect_equal(owner, "org")
      expect_equal(repo, "repo")
      mock_release
    },
    fetch_open_milestones = function(owner, repo, token) {
      expect_equal(owner, "org")
      expect_equal(repo, "repo")
      mock_milestones
    },
    fetch_branch_comparison = function(owner, repo, base, head, token) {
      expect_equal(list(owner = owner, repo = repo, base = base, head = head),
                   list(owner = "org", repo = "repo", base = "main", head = "dev"))
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
})

test_that("summarize_github_repos appends qualification badge when registry matches", {
  mock_release <- list(
    tag_name = "v1.0.0",
    published_at = "2025-01-01T12:00:00Z",
    html_url = "https://github.com/org/repo/releases/tag/v1.0.0"
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
    fetch_latest_release = function(...) mock_release,
    fetch_open_milestones = function(...) list(),
    fetch_branch_comparison = function(...) list(ahead_by = 0, behind_by = 0)
  )

  expect_match(result$latest_release, "128737")
  expect_match(result$latest_release, "qualification_v1_0_0")
})

test_that("summarize_github_repos shows grey badge when older version qualified", {
  mock_release <- list(
    tag_name = "v1.1.0",
    published_at = "2025-02-01T12:00:00Z",
    html_url = "https://github.com/org/repo/releases/tag/v1.1.0"
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
    fetch_latest_release = function(...) mock_release,
    fetch_open_milestones = function(...) list(),
    fetch_branch_comparison = function(...) list(ahead_by = 0, behind_by = 0)
  )

  expect_match(result$latest_release, "badge--slate")
  expect_match(result$latest_release, "128737")
  expect_match(result$latest_release, "qualification_v1_0_0")
})

test_that("summarize_github_repos supports multiple repositories", {
  releases <- list(
    list(
      tag_name = "v1.1.0",
      published_at = "2025-02-15T00:00:00Z",
      html_url = "https://github.com/org/repo/releases/tag/v1.1.0"
    ),
    NULL
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
    fetch_latest_release = function(owner, repo, token) {
      index <<- index + 1
      releases[[index]]
    },
    fetch_open_milestones = function(owner, repo, token) {
      milestones[[index]]
    },
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
})

