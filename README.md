# gh.dash

{gh.dash} is an R package that creates a simple dashboad that summarizes the development status for Github repos. 

## Published report

The report is rendered nightly by GitHub Actions and deployed to the `gh-pages` branch so that it is served at <https://gilead-biostats.github.io/gh.dash/>. Each run overwrites the `report/` directory on the `gh-pages` branch to keep the published HTML current. Fork the package and update `inst/extdata/pacakge-list.csv` to run the report for your custom list of packages.  

## Local development

To render the package status report locally, run the helper script:

```sh
Rscript inst/examples/RenderReport.R
```

The script loads the package (using `pkgload::load_all()` when needed) and writes the HTML output to `inst/examples/output/index.html`. Open that file in a browser to preview changes. The document is self-contained, so no additional assets are required when publishing.

## GitHub Actions

Two workflows keep the hosted reports current:

- `.github/workflows/render-package-status-report.yaml` handles pushes to `main` and `dev`, pull requests, and manual `workflow_dispatch` runs. Each job calls the reusable workflow in `.github/workflows/render-report-reusable.yaml`, passing the branch-specific ref, output directory, and the package list (as an R vector string, e.g. `c("org/repo1", "org/repo2")`). Main publishes to `report/`, dev publishes to `report/dev/`, and pull requests publish to `report/pr/<number>/` while also posting a comment that links to the preview site.
- `.github/workflows/render-package-status-report-scheduled.yaml` runs nightly at 05:00 UTC. It invokes the reusable workflow twice—first for `main`, then for `dev`—to refresh both environments even when there are no new commits during the day.

Manual runs are available through the *Run workflow* button on `render-package-status-report`. Select either the `main` or `dev` target; trigger two runs back-to-back whenever both environments need rebuilding. Pull requests originating from forks are skipped automatically during deployment because the workflows rely on the runtime `github.token`.

### Required secrets

The workflows optionally use the following repository secret:

- `GH_DASH_REPOS_TOKEN` (optional): A PAT with `repo` scope for accessing private repositories. If not provided, the workflow falls back to the default `github.token`, which only has access to public repositories.
- `GH_QUAL_REGISTRY_TOKEN` (optional): A PAT with access to `Gilead-BioStats/r-qualification` when qualification metadata is needed. If not provided, qualification data is skipped.

