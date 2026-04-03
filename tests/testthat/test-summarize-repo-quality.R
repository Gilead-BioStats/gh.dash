test_that("count_test_that_calls counts test_that invocations", {
  expect_equal(gh.dash:::count_test_that_calls(NULL), 0L)
  expect_equal(gh.dash:::count_test_that_calls(""), 0L)
  expect_equal(
    gh.dash:::count_test_that_calls("test_that('a', {});\n test_that ('b', {})"),
    2L
  )
})

test_that("summarize_repo_quality validates repository format", {
  expect_error(gh.dash:::summarize_repo_quality(42), "character vector")
  expect_error(gh.dash:::summarize_repo_quality(character(0)), "at least one")
  expect_error(gh.dash:::summarize_repo_quality(c("")), "cannot contain empty strings")
  expect_error(gh.dash:::summarize_repo_quality(c("invalid")), "owner/repo")
})

test_that("summarize_repo_quality reports test count and qcthat status", {
  result <- with_mocked_bindings(
    gh.dash:::summarize_repo_quality("org/repo"),
    .package = "gh.dash",
    fetch_repo_metadata = function(...) list(private = FALSE, default_branch = "main"),
    fetch_repo_git_tree = function(...) list(
      tree = list(
        list(path = "tests/testthat/test-one.R"),
        list(path = "tests/testthat/test-two.R"),
        list(path = ".github/workflows/qcthat.yaml")
      )
    ),
    fetch_repo_file_content = function(owner, repo, path, ref, token) {
      content <- switch(
        path,
        "tests/testthat/test-one.R" = "test_that('a', {})\n",
        "tests/testthat/test-two.R" = "test_that('b', {})\n test_that('c', {})\n",
        ""
      )
      list(content = base64enc::base64encode(charToRaw(content)))
    }
  )

  expect_s3_class(result, "data.frame")
  expect_equal(result$repo, "<a href=\"https://github.com/org/repo\">org/repo</a>")
  expect_equal(result$test_count, 3L)
  expect_equal(result$qcthat_status, "Yes")
})

test_that("summarize_repo_quality handles missing tree data", {
  result <- with_mocked_bindings(
    gh.dash:::summarize_repo_quality("org/repo"),
    .package = "gh.dash",
    fetch_repo_metadata = function(...) list(private = FALSE, default_branch = "main"),
    fetch_repo_git_tree = function(...) NULL,
    fetch_repo_file_content = function(...) stop("should not be called")
  )

  expect_equal(result$test_count, NA_integer_)
  expect_equal(result$qcthat_status, "Unavailable")
})

test_that("summarize_repo_quality handles truncated tree", {
  result <- with_mocked_bindings(
    gh.dash:::summarize_repo_quality("org/repo"),
    .package = "gh.dash",
    fetch_repo_metadata = function(...) list(private = FALSE, default_branch = "main"),
    fetch_repo_git_tree = function(...) list(truncated = TRUE, tree = list()),
    fetch_repo_file_content = function(...) stop("should not be called")
  )

  expect_equal(result$test_count, NA_integer_)
  expect_equal(result$qcthat_status, "Unavailable")
})

test_that("summarize_repo_quality returns NA test_count when a file fetch fails", {
  result <- with_mocked_bindings(
    gh.dash:::summarize_repo_quality("org/repo"),
    .package = "gh.dash",
    fetch_repo_metadata = function(...) list(private = FALSE, default_branch = "main"),
    fetch_repo_git_tree = function(...) list(
      tree = list(
        list(path = "tests/testthat/test-one.R"),
        list(path = "tests/testthat/test-two.R")
      )
    ),
    fetch_repo_file_content = function(owner, repo, path, ref, token) {
      if (path == "tests/testthat/test-one.R") {
        list(content = base64enc::base64encode(charToRaw("test_that('a', {})\n")))
      } else {
        NULL  # simulate API failure for second file
      }
    }
  )

  expect_equal(result$test_count, NA_integer_)
  expect_equal(result$qcthat_status, "No")
})
