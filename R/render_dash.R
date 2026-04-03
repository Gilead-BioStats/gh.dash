#' Render a local gh.dash HTML report
#'
#' Renders the package status report locally using the gh.dash report template.
#'
#' @param packages Character vector of repository slugs (e.g. "org/repo").
#' @param output_dir Directory where the report should be written.
#' @param output_file Output filename for the rendered report.
#' @param title Title for the report.
#' @param token GitHub personal access token (optional).
#' @param qualification_registry_url URL or file path to the qualification registry CSV (optional).
#' @param pr_activity_days Number of days to include in PR activity summaries (default: 365).
#' @param include_quality Logical. When `TRUE` (the default), the Quality tab is
#'   computed and rendered. Set to `FALSE` to skip the Quality tab and avoid the
#'   additional GitHub API requests it requires.
#' @param clean Whether to clean intermediate files after rendering.
#'
#' @return The path to the rendered HTML report (invisibly).
#' @export
render_dash <- function(
  packages,
  output_dir = "report",
  output_file = "index.html",
  title = "Package",
  token = NULL,
  qualification_registry_url = NULL,
  pr_activity_days = 365,
  include_quality = TRUE,
  clean = TRUE
) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package 'rmarkdown' is required to render the report.", call. = FALSE)
  }

  if (missing(packages) || is.null(packages)) {
    packages <- character(0)
  }

  packages <- as.character(packages)
  packages <- trimws(packages)
  packages <- packages[nzchar(packages)]
  pr_activity_days <- validate_lookback_days(pr_activity_days)

  report_path <- system.file("report", "package_status_report.Rmd", package = "gh.dash")
  if (!nzchar(report_path)) {
    stop("Could not locate gh.dash report template.", call. = FALSE)
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  rmarkdown::render(
    input = report_path,
    output_dir = output_dir,
    output_file = output_file,
    params = list(
      token = token,
      title = title,
      packageList = packages,
      qualification_registry_url = qualification_registry_url,
      pr_activity_days = pr_activity_days,
      include_quality = include_quality
    ),
    clean = clean
  )

  output_path <- normalizePath(file.path(output_dir, output_file), mustWork = FALSE)
  invisible(output_path)
}