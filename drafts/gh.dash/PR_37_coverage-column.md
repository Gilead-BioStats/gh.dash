<!-- STATUS: Posted to https://github.com/Gilead-BioStats/gh.dash/pull/37 on 2026-04-07 -->
<!-- GITHUB_PROPERTIES: Assignee: @me -->

This PR was drafted by GitHub Copilot using Claude Sonnet 4.6 and reviewed by @nandriychuk

## Summary

Adds a **Coverage** column to the Quality tab of the gh.dash dashboard. The column displays the latest unit test coverage percentage for each repository, sourced from a `coverage-summary.json` asset attached to the latest GitHub release.

Closes #28

## Changes

### `R/github_api.R`
- New `fetch_release_asset_content(owner, repo, asset_id, token)` — downloads a release asset by ID using the GitHub API and returns its content as a character string (or `NULL` on failure).

### `R/summarize_repo_quality.R`
- New `fetch_coverage_percent(owner, repo, token)` — uses the existing `fetch_releases()` + `derive_latest_release()` pipeline to find the latest non-draft, non-prerelease release, then locates a `coverage-summary.json` asset, downloads it, and parses `coverage_percent`. Returns `list(coverage_percent, release_url)` with `NA` for coverage on any failure.
- New `format_coverage_summary(coverage_percent, release_url)` — formats the value as `"84.7%"` (linked to the release page when available) or `"Unavailable"` for `NA`.
- `summarize_repo_quality()` updated to call `fetch_coverage_percent()` and include `coverage` as the first column of the output data frame (before `test_count` and `qcthat_status`).

### `inst/report/package_status_report.Rmd`
- `render_quality_table()` updated to render the new `coverage` column first (after Repository), with header "Coverage".
- Added a **column-settings toolbar** (matching the Repository Status tab) to the Quality table, allowing users to show/hide individual columns.

### `DESCRIPTION`
- Added `jsonlite` to `Imports` (used to parse `coverage-summary.json`).

## Test Coverage

- 12 new tests in `tests/testthat/test-coverage.R`:
  - `fetch_coverage_percent`: happy path, no release, no matching asset, unparseable JSON, failed download
  - `format_coverage_summary`: linked percentage (1 decimal), plain text when no URL, `"Unavailable"` for `NA`
  - `summarize_repo_quality`: coverage column present, correct format, `"Unavailable"` when no data
- All 218 tests pass; 0 errors, 0 warnings in `devtools::check()`.
