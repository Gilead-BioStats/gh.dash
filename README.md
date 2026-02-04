# gh.dash

{gh.dash} is an R package that creates a simple dashboard that summarizes the development status for Github repos. The user provides a list of pacakges to include and the pacakge does the following: 

- Pulls data about package releases, milestones and, optionally qualification status using the github API
- Creates a report summarizing the status for each pacakge using R markdown
- Uses pre-configured GitHub actions to automatically pushes the report to Github Pages

The {gh.dash} repo automatically runs a sample report for tidyverse packages, but the package can be configured to work with any GitHub repos using the steps below. 

# Configuration

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

## Setting up Automated Dashboards

You can set up a repo that will automatically post the gh.dash report to github pages by following thse steps:

1. Create a .csv package list with a `repo` column (recommended: `/pkg_list.csv`).
2. Set up environmental variables
    - Set a repository variable called `PKGLISTPATH` pointing to the csv if it isn't saved as `/pkg_list.csv`. 
    - Set a repository (or organization) secret `GH_DASH_REPOS` if you need access to private repositories. See below for guidance on fine-grained permissions. 
4. Enable GitHub Pages (deploys to `gh-pages`) if you want the report to be hosted.
5. Add a workflow  (e.g. `.github/workflows/render-package-status-report.yaml`) using the template below: 

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

# Technical Details

## Environmental Variables

The workflows optionally use the following repository secret:

- `GH_DASH_REPOS` (optional): A fine-grained PAT used to access private repositories and (optionally) the qualification registry. If not provided, the workflow falls back to the default `github.token`, which only has access to public repositories.

If you use a fine-grained PAT for `GH_DASH_REPOS`, grant access to the repositories you want to report on and the following repository permissions:

- Contents: Read
- Metadata: Read
- Issues: Read (for issue/milestone data)

## GitHub Actions

Two workflows keep the hosted reports current:

- `.github/workflows/render-package-status-report.yaml` handles pushes to `main` and `dev`, pull requests, and manual `workflow_dispatch` runs. Each job calls the reusable workflow in `.github/workflows/render-report-reusable.yaml`, passing the branch-specific ref, output directory, and the package list (as an R vector string, e.g. `c("org/repo1", "org/repo2")`). Main publishes to `report/`, dev publishes to `report/dev/`, and pull requests publish to `report/pr/<number>/` while also posting a comment that links to the preview site.
- `.github/workflows/render-package-status-report-scheduled.yaml` runs nightly at 05:00 UTC. It invokes the reusable workflow twice—first for `main`, then for `dev`—to refresh both environments even when there are no new commits during the day.

Manual runs are available through the *Run workflow* button on `render-package-status-report`. Select either the `main` or `dev` target; trigger two runs back-to-back whenever both environments need rebuilding. Pull requests originating from forks are skipped automatically during deployment because the workflows rely on the runtime `github.token`.

