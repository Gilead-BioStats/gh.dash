# gh.dash 1.0.0

## Overview

Initial release of `{gh.dash}`, an R package that generates an HTML dashboard for tracking the development status of a portfolio of GitHub repositories. Given a list of repository slugs and a GitHub API token, the package queries the GitHub API for each repository and renders a self-contained R Markdown report that is suitable for deployment to GitHub Pages.

The dashboard contains two tabs:

- **Repository Status** — a table summarising the latest release, open milestones, open pull requests, branch divergence, issue counts, and release cadence for every tracked repository.
- **PR Activity** — a per-user breakdown of pull requests opened and reviewed over a configurable lookback window (default: 365 days).

---

## New features

### Core rendering

- Added `render_dash()` as the primary user-facing entry point. It accepts a vector of `owner/repo` slugs together with optional parameters (output directory, report title, lookback window, qualification registry URL) and delegates to `rmarkdown::render()` to produce a self-contained HTML report (`inst/report/package_status_report.Rmd`).

### Repository Status tab

- **Repository column** — displays a hyperlink to each repository on GitHub, with a lock icon appended for private repositories.
- **Latest release column** — shows the most recent release tag and publication date. When a qualification registry is configured, an optional qualification badge is rendered alongside the release tag.
- **Open milestones column** — renders milestone badges with completion percentages and open/total issue counts, complete with hover tooltips.
- **Open PRs column** — displays the count of open pull requests as a hyperlink to the repository's pull-requests page.
- **Dev status column** — shows the ahead/behind commit difference between the default branch and the development branch (`+N / -N` format) so that unreleased work is immediately visible. A header tooltip explains the column semantics.
- **Issues column** — displays the ratio of open to closed issues per repository.
- **\# Releases column** — counts all releases in the repository's lifetime and includes a hover tooltip showing the number of releases in the past 90 days. The column replaced an earlier 365-day window after feedback that a lifetime count is more meaningful.

### PR Activity tab

- Added `summarize_pr_activity_by_user()`, which aggregates pull request review activity across all tracked repositories and returns a per-user summary table.
- The aggregation is cache-first: review data is written to a local cache directory per repository and read back on subsequent runs to avoid redundant API calls. Cache paths are built via `build_pr_review_cache_path()` / `sanitize_cache_component()`, and cache I/O is handled by `read_pr_review_cache()` / `write_pr_review_cache()`.
- The PR Activity tab also surfaces **open active PRs** and a **PRs pending review** column for a real-time view of work in flight.
- Hover tooltips on PR Activity links enumerate the affected repositories.

### Infrastructure

- Included a reusable GitHub Actions workflow (`render-report-reusable.yaml`) that installs R, fetches the package list from `inst/extdata/package-list.csv`, optionally enriches with qualification data, renders the R Markdown dashboard, and deploys the resulting HTML to GitHub Pages.
- `render-package-status-report.yaml` calls the reusable workflow on push to `main` and `dev`, on pull requests (deploying previews to `pr/<number>/`), on `workflow_dispatch`, and on a nightly schedule at 05:00 UTC.
- Concurrency controls prevent parallel workflow runs from overwriting each other's GitHub Pages deployments.
- `inst/extdata/package-list.csv` — CSV file committed to the repository as the default package list, making it easy to customise which repositories are tracked without modifying workflow YAML.
- `inst/examples/RenderReport.R` — walkthrough script demonstrating local report rendering.
