# gh.dash

{gh.dash} is an R package that creates a simple dashboard that summarizes the development status for GitHub repos. The user provides a list of packages to include and the package does the following:

- Pulls data about package releases, milestones and, optionally qualification status using the GitHub API
- Creates a report summarizing the status for each package using R Markdown
- Uses pre-configured GitHub Actions to automatically push the report to GitHub Pages

The {gh.dash} repo automatically runs a sample report for tidyverse packages, but the package can be configured to work with any GitHub repos using the steps below. 

# Configuration

## GitHub Actions Overview

The {gh.dash} package includes reusable GitHub Actions workflows that automate report generation and deployment. 
- The main workflow (`render-report-reusable.yaml`) orchestrates the entire process: it checks out your repository, installs R and dependencies, fetches package data from the GitHub API using your package list CSV, optionally enriches the data with qualification status from a registry repository, renders the dashboard report using R Markdown, and deploys the resulting HTML to GitHub Pages. You can configure workflows to run on different triggers (push, pull request, scheduled) and deploy to different subdirectories on `gh-pages` (e.g., `main` branch to root, `dev` branch to `/dev`, pull requests to `/pr/{number}`). This automation ensures your dashboard stays up-to-date without manual intervention—simply push changes or wait for the scheduled run, and your stakeholders will see the latest package status at your GitHub Pages URL.

One workflow keeps the hosted reports current:
- `render-package-status-report.yaml` handles pushes to main and dev, pull requests, manual workflow_dispatch runs, and a nightly scheduled run at 05:00 UTC. Each job calls the reusable workflow in render-report-reusable.yaml, passing the branch-specific ref and output directory. Main publishes to the root of GitHub Pages, dev publishes to the dev/ subdirectory, and pull requests publish to pr/<number>/ while also posting a comment that links to the preview site. The scheduled run refreshes both environments even when there are no new commits during the day.
## Running a Single Report

Use `render_dash()` to generate a local HTML report from an R session:

```r
library(gh.dash)

render_dash(
	packages = c(
		"tidyverse/ggplot2",
		"tidyverse/dplyr",
		"tidyverse/tidyr"
	),
	output_dir = "report",
	output_file = "index.html",
	title = "Tidyverse"
)
```

Additionally, you can render a report from the terminal.

```sh
Rscript inst/examples/RenderReport.R
```
The script assumes the `{gh.dash}` package is installed and writes the HTML output to `inst/examples/output/index.html`. Open that file in a browser to preview changes. The document is self-contained, so no additional assets are required when publishing.

## Setting up Automated Dashboards

You can set up a repo that will automatically post the gh.dash report to GitHub Pages by following these steps:

1. Create a CSV package list with a `repo` column (recommended: `/pkg_list.csv`).
2. Set a repository (or organization) PAT/secret `GH_DASH_REPOS` if you need access to private repositories. See below for details about fine-grained permissions.  
3. Enable GitHub Pages (deploys to `gh-pages`) if you want the report to be hosted.
4. Add a workflow (e.g. `.github/workflows/render-package-status-report.yaml`) to run the {gh.dash} action. See below for a template. 
5. Update the `with:` section of the yaml to match the configuration of your repo. See below for config details. 


# Technical Details

## GitHub Action template

The following template can be customized to automatically run the {gh.dash} report in different repos. 

```yaml
name: render-package-status-report

on:
	workflow_dispatch:
	push:
		branches: [main]

jobs:
	render:
		uses: Gilead-BioStats/gh.dash/.github/workflows/render-report-reusable.yaml@main
		with:
			ref: ${{ github.ref_name }}
			pkg-list-path: "pkg_list.csv"
			output-subdir: ""
			deploy: true
			deploy-target: ""
			deploy-clean: false
			deploy-clean-exclude: ""
		secrets:
			GH_DASH_REPOS: ${{ secrets.GH_DASH_REPOS }}
```

The following inputs can be customized using the `with:` section. 

- `title` : Report title displayed in the dashboard.
- `pkg-list-path` (optional): Path to a CSV file with a `repo` column containing repository slugs (default: `pkg_list.csv`).
- `qual-repo` (optional): Qualification registry repository slug (e.g. `Gilead-BioStats/r-qualification`). 
- `qual-path` (optional): Path to the qualification registry CSV within the repo (e.g. `qualified-releases.csv`).
- `output-subdir` (optional): Subdirectory for output (e.g. `dev`).
- `deploy` (optional): Whether to deploy the rendered report to `gh-pages` (default: true).
- `deploy-target` (optional): Target folder on `gh-pages` to deploy into.
- `deploy-clean` (optional): Whether to clean the target folder before deploy (default: false).
- `deploy-clean-exclude` (optional): Newline-delimited patterns to preserve when cleaning.

For a slightly more complex implementation of GHA deploy see `.github/workflows/render-package-status-report.yaml`. 

## PAT for GitHub API

The workflows optionally use the following repository secret:

- `GH_DASH_REPOS` (optional): A fine-grained PAT used to access private repositories and (optionally) the qualification registry. If not provided, the workflow falls back to the default `github.token`, which only has access to public repositories.

If you use a fine-grained PAT for `GH_DASH_REPOS`, grant access to the repositories you want to report on and the following repository permissions:

- Contents: Read
- Metadata: Read
- Issues: Read (for issue/milestone data)