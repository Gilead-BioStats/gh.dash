# --- fetch_coverage_percent ---

test_that("fetch_coverage_percent returns coverage_percent and release_url #28", {
  result <- with_mocked_bindings(
    gh.dash:::fetch_coverage_percent("org", "repo", token = NULL),
    .package = "gh.dash",
    fetch_releases = function(...) {
      list(
        list(
          tag_name = "v1.0.0",
          html_url = "https://github.com/org/repo/releases/tag/v1.0.0",
          draft = FALSE,
          prerelease = FALSE,
          assets = list(
            list(url = "https://api.github.com/repos/org/repo/releases/assets/123", name = "coverage-summary.json")
          )
        )
      )
    },
    fetch_release_asset_content = function(url, token) {
      '{"coverage_percent": 84.7}'
    }
  )
  expect_equal(result$coverage_percent, 84.7)
  expect_equal(result$release_url, "https://github.com/org/repo/releases/tag/v1.0.0")
})

test_that("fetch_coverage_percent returns NA when no latest release found #28", {
  result <- with_mocked_bindings(
    gh.dash:::fetch_coverage_percent("org", "repo", token = NULL),
    .package = "gh.dash",
    fetch_releases = function(...) NULL,
    fetch_release_asset_content = function(...) stop("should not be called")
  )
  expect_true(is.na(result$coverage_percent))
  expect_null(result$release_url)
})

test_that("fetch_coverage_percent returns NA when no coverage-summary.json asset #28", {
  result <- with_mocked_bindings(
    gh.dash:::fetch_coverage_percent("org", "repo", token = NULL),
    .package = "gh.dash",
    fetch_releases = function(...) {
      list(
        list(
          tag_name = "v1.0.0",
          html_url = "https://github.com/org/repo/releases/tag/v1.0.0",
          draft = FALSE,
          prerelease = FALSE,
          assets = list(list(url = "https://api.github.com/repos/org/repo/releases/assets/456", name = "other-asset.zip"))
        )
      )
    },
    fetch_release_asset_content = function(...) stop("should not be called")
  )
  expect_true(is.na(result$coverage_percent))
  expect_equal(result$release_url, "https://github.com/org/repo/releases/tag/v1.0.0")
})

test_that("fetch_coverage_percent returns NA for unparseable asset content #28", {
  result <- with_mocked_bindings(
    gh.dash:::fetch_coverage_percent("org", "repo", token = NULL),
    .package = "gh.dash",
    fetch_releases = function(...) {
      list(
        list(
          tag_name = "v1.0.0",
          html_url = "https://github.com/org/repo/releases/tag/v1.0.0",
          draft = FALSE,
          prerelease = FALSE,
          assets = list(list(url = "https://api.github.com/repos/org/repo/releases/assets/789", name = "coverage-summary.json"))
        )
      )
    },
    fetch_release_asset_content = function(...) "not valid json {{{"
  )
  expect_true(is.na(result$coverage_percent))
  expect_equal(result$release_url, "https://github.com/org/repo/releases/tag/v1.0.0")
})

test_that("fetch_coverage_percent returns NA when asset download fails #28", {
  result <- with_mocked_bindings(
    gh.dash:::fetch_coverage_percent("org", "repo", token = NULL),
    .package = "gh.dash",
    fetch_releases = function(...) {
      list(
        list(
          tag_name = "v1.0.0",
          html_url = "https://github.com/org/repo/releases/tag/v1.0.0",
          draft = FALSE,
          prerelease = FALSE,
          assets = list(list(url = "https://api.github.com/repos/org/repo/releases/assets/789", name = "coverage-summary.json"))
        )
      )
    },
    fetch_release_asset_content = function(...) NULL
  )
  expect_true(is.na(result$coverage_percent))
  expect_equal(result$release_url, "https://github.com/org/repo/releases/tag/v1.0.0")
})

# --- format_coverage_summary ---

test_that("format_coverage_summary formats as linked percentage with one decimal place #28", {
  result <- gh.dash:::format_coverage_summary(84.7, "https://github.com/org/repo/releases/tag/v1.0.0")
  expect_match(result, "84.7%")
  expect_match(result, 'href="https://github.com/org/repo/releases/tag/v1.0.0"')
})

test_that("format_coverage_summary rounds to one decimal place #28", {
  result <- gh.dash:::format_coverage_summary(58.7477, NULL)
  expect_match(result, "58.7%")
  expect_false(grepl("58.74", result, fixed = TRUE))
})

test_that("format_coverage_summary returns plain text when release URL is NULL #28", {
  result <- gh.dash:::format_coverage_summary(84.7, NULL)
  expect_match(result, "84.7%")
  expect_false(grepl("<a", result, fixed = TRUE))
})

test_that("format_coverage_summary returns Unavailable for NA coverage #28", {
  expect_equal(gh.dash:::format_coverage_summary(NA_real_, NULL), "Unavailable")
  expect_equal(gh.dash:::format_coverage_summary(NA_real_, "https://x.com"), "Unavailable")
})

# --- summarize_repo_quality: coverage column ---

test_that("summarize_repo_quality output includes coverage column #28", {
  result <- with_mocked_bindings(
    gh.dash:::summarize_repo_quality("org/repo"),
    .package = "gh.dash",
    fetch_repo_metadata = function(...) list(private = FALSE, default_branch = "main"),
    fetch_repo_git_tree = function(...) list(tree = list()),
    fetch_repo_file_content = function(...) NULL,
    fetch_coverage_percent = function(...) {
      list(
        coverage_percent = 84.7,
        release_url = "https://github.com/org/repo/releases/tag/v1.0.0"
      )
    }
  )
  expect_true("coverage" %in% names(result))
})

test_that("summarize_repo_quality formats coverage as linked percentage #28", {
  result <- with_mocked_bindings(
    gh.dash:::summarize_repo_quality("org/repo"),
    .package = "gh.dash",
    fetch_repo_metadata = function(...) list(private = FALSE, default_branch = "main"),
    fetch_repo_git_tree = function(...) list(tree = list()),
    fetch_repo_file_content = function(...) NULL,
    fetch_coverage_percent = function(...) {
      list(
        coverage_percent = 84.7,
        release_url = "https://github.com/org/repo/releases/tag/v1.0.0"
      )
    }
  )
  expect_match(result$coverage, "84.7%")
  expect_match(result$coverage, 'href="https://github.com/org/repo/releases/tag/v1.0.0"')
})

test_that("summarize_repo_quality shows Unavailable when coverage data is missing #28", {
  result <- with_mocked_bindings(
    gh.dash:::summarize_repo_quality("org/repo"),
    .package = "gh.dash",
    fetch_repo_metadata = function(...) list(private = FALSE, default_branch = "main"),
    fetch_repo_git_tree = function(...) list(tree = list()),
    fetch_repo_file_content = function(...) NULL,
    fetch_coverage_percent = function(...) {
      list(
        coverage_percent = NA_real_,
        release_url = NULL
      )
    }
  )
  expect_equal(result$coverage, "Unavailable")
})

# --- fetch_coverage_percent: releases argument (#39) ---

test_that("fetch_coverage_percent uses provided releases arg and skips fetch_releases #39", {
  pre_fetched <- list(
    list(
      tag_name = "v2.0.0",
      html_url = "https://github.com/org/repo/releases/tag/v2.0.0",
      draft = FALSE,
      prerelease = FALSE,
      assets = list(
        list(
          url = "https://api.github.com/repos/org/repo/releases/assets/999",
          name = "coverage-summary.json"
        )
      )
    )
  )
  result <- with_mocked_bindings(
    gh.dash:::fetch_coverage_percent("org", "repo", token = NULL, releases = pre_fetched),
    .package = "gh.dash",
    fetch_releases = function(...) stop("fetch_releases must not be called when releases provided #39"),
    fetch_release_asset_content = function(url, token) '{"coverage_percent": 90.5}'
  )
  expect_equal(result$coverage_percent, 90.5)
  expect_equal(result$release_url, "https://github.com/org/repo/releases/tag/v2.0.0")
})
