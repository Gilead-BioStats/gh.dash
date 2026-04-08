<!-- STATUS: Drafted on 2026-04-07 -->
<!-- GITHUB_PROPERTIES: Labels: enhancement, Assignee: @me -->

This Issue was drafted by GitHub Copilot using Claude Sonnet 4.6 and reviewed by @nandriychuk

# Allow Users to Configure Visible Tabs and Hidden Columns per Tab

## Summary

Users cannot currently control which tabs appear in the dashboard or hide columns within the PR Activity and Quality tabs. The Repository Status tab already has an interactive column-visibility toggle, but the other tabs lack this feature. In addition, there is no way to exclude specific tabs (e.g., PR Activity) at render time — the only tab-level control is `include_quality`.

This issue proposes two related improvements:

1. **Tab selection** — Add a `tabs` parameter to `render_dash()` that lets users specify which tabs to include (e.g., `tabs = c("repo-status", "pr-activity", "quality")`), superseding the existing `include_quality` flag.
2. **Column hiding per tab** — Add the same interactive column-settings toolbar (already present on the Repository Status tab) to the PR Activity and Quality tabs.

## Proposed Solution

### Tab Selection (`tabs` parameter)

Add a `tabs` character vector parameter to `render_dash()` (and the corresponding Rmd `params`) with a default that preserves current behavior:

```r
render_dash(
  packages = ...,
  tabs = c("repo-status", "pr-activity")  # default: repo status and PR activity
)
```

- Valid values: `"repo-status"`, `"pr-activity"`, `"quality"`.
- Default is `c("repo-status", "pr-activity")` — Quality is opt-in.
- If `"quality"` is included, perform the Quality API calls; otherwise skip them (same behavior as `include_quality = FALSE` today).
- Deprecate `include_quality` in favor of `tabs`.
- The first tab in the vector becomes the initially-active tab.

### Column Hiding for PR Activity and Quality Tabs

Extend `render_pr_activity_table()` and `render_quality_table()` to wrap their tables in the same `.status-table-wrapper` structure that `render_status_table()` uses, including:

- The column-settings button / panel with checkboxes for each column.
- The existing JavaScript that drives the toggle behavior already handles all `.status-table-wrapper` elements generically, so no JS changes should be needed.

**PR Activity columns:** User, PRs Opened (Active), PRs Reviewed, PRs Pending Review, Created/Reviewed

**Quality columns:** Repository, # of tests, uses qcthat

## Example Usage

```r
# Show only repo status and quality tabs; PR Activity excluded
render_dash(
  packages = c("org/repo1", "org/repo2"),
  tabs = c("repo-status", "quality")
)

# Default (repo status + PR activity)
render_dash(
  packages = c("org/repo1", "org/repo2")
)

# All tabs
render_dash(
  packages = c("org/repo1", "org/repo2"),
  tabs = c("repo-status", "pr-activity", "quality")
)
```

## QC Approach

In addition to standard QC (e.g., code reviews, automated checks) the following QC measures will be implemented:

- [ ] Unit Tests
- [ ] User Tests (e.g. visual comparison)
