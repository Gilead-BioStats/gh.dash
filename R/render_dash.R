#' Check whether Quarto CLI (>= 1.4) and the quarto R package are available
#'
#' @return `TRUE` if usable, `FALSE` otherwise.
#' @noRd
has_quarto_support <- function() {
  if (!requireNamespace("quarto", quietly = TRUE)) {
    return(FALSE)
  }

  tryCatch(
    {
      path <- quarto::quarto_path()
      if (is.null(path) || !nzchar(path)) {
        return(FALSE)
      }

      version <- quarto::quarto_version()
      if (is.null(version)) {
        return(FALSE)
      }

      version >= numeric_version("1.4")
    },
    error = function(e) FALSE
  )
}

#' Render a local gh.dash HTML report
#'
#' Renders the package status report locally using the gh.dash report template.
#'
#' When the \pkg{quarto} R package and the Quarto CLI (>= 1.4) are available the
#' Quarto dashboard template is used.
#' Otherwise the function falls back to the legacy \pkg{rmarkdown} template.
#' At least one of the two must be available.
#'
#' @param packages Character vector of repository slugs (e.g. "org/repo").
#' @param output_dir Directory where the report should be written.
#' @param output_file Output filename for the rendered report.
#' @param title Title for the report.
#' @param token GitHub personal access token (optional).
#' @param qualification_registry_url URL or file path to the qualification registry CSV (optional).
#' @param clean Whether to clean intermediate files after rendering.
#'   Only used with the legacy rmarkdown engine; ignored when rendering via Quarto.
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
  clean = TRUE
) {
  use_quarto <- has_quarto_support()

  if (!use_quarto && !requireNamespace("rmarkdown", quietly = TRUE)) {
    stop(
      "Neither Quarto nor rmarkdown is available.\n",
      "Install Quarto CLI (https://quarto.org/docs/get-started/) and the ",
      "'quarto' R package, or install the 'rmarkdown' package to render the report.",
      call. = FALSE
    )
  }

  if (missing(packages) || is.null(packages)) {
    packages <- character(0)
  }

  packages <- as.character(packages)
  packages <- trimws(packages)
  packages <- packages[nzchar(packages)]

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  render_params <- list(
    token = token,
    title = title,
    packageList = packages,
    qualification_registry_url = qualification_registry_url
  )

  if (use_quarto) {
    report_path <- system.file("report", "package_status_report.qmd", package = "gh.dash")
    if (!nzchar(report_path)) {
      stop("Could not locate gh.dash Quarto report template.", call. = FALSE)
    }

    css_path <- system.file("report", "styles.css", package = "gh.dash")

    # Quarto renders output next to input — copy template to a writable temp dir
    tmp_dir <- tempfile("gh_dash_quarto_")
    dir.create(tmp_dir, recursive = TRUE)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    tmp_report <- file.path(tmp_dir, basename(report_path))
    file.copy(report_path, tmp_report)

    # Copy the external CSS alongside the template so the relative ref resolves
    if (nzchar(css_path)) {
      file.copy(css_path, file.path(tmp_dir, "styles.css"))
    }

    quarto::quarto_render(
      input = tmp_report,
      output_file = output_file,
      execute_params = render_params
    )

    rendered_file <- file.path(tmp_dir, output_file)
    output_path <- file.path(output_dir, output_file)
    file.copy(rendered_file, output_path, overwrite = TRUE)
    output_path <- normalizePath(output_path, mustWork = FALSE)
  } else {
    report_path <- system.file("report", "package_status_report.Rmd", package = "gh.dash")
    if (!nzchar(report_path)) {
      stop("Could not locate gh.dash report template.", call. = FALSE)
    }

    rmarkdown::render(
      input = report_path,
      output_dir = output_dir,
      output_file = output_file,
      params = render_params,
      clean = clean
    )

    output_path <- normalizePath(file.path(output_dir, output_file), mustWork = FALSE)
  }

  invisible(output_path)
}
