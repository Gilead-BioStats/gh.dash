#!/usr/bin/env Rscript
# Render the tidyverse demo package status report into a local report/ directory.

if (!requireNamespace("rmarkdown", quietly = TRUE)) {
  stop("Package 'rmarkdown' is required to render the report.", call. = FALSE)
}

args <- commandArgs(trailingOnly = FALSE)
file_flag <- "--file="
script_path <- args[grepl(file_flag, args, fixed = TRUE)]
if (length(script_path)) {
  script_path <- sub(file_flag, "", script_path, fixed = TRUE)
  script_dir <- dirname(normalizePath(script_path))
} else {
  script_dir <- getwd()
}

package_root <- normalizePath(file.path(script_dir, "..", ".."), mustWork = TRUE)
report_dir <- normalizePath(file.path(package_root, "inst", "report"), mustWork = TRUE)
output_dir <- normalizePath(file.path(script_dir, "output"), mustWork = FALSE)
if (dir.exists(output_dir)) {
  unlink(output_dir, recursive = TRUE)
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(package_root, quiet = TRUE)
} else {
  library(gh.dash)
}

rmarkdown::render(
  input = file.path(report_dir, "package_status_report.Rmd"),
  output_dir = output_dir,
  output_file = "index.html",
  params = list(
    token = Sys.getenv("GITHUB_TOKEN", unset = ""),
    title = "Tidyverse",
    packageList = c(
      "tidyverse/ggplot2",
      "tidyverse/dplyr",
      "tidyverse/tidyr",
      "tidyverse/readr",
      "tidyverse/purrr",
      "tidyverse/tibble",
      "tidyverse/stringr",
      "tidyverse/forcats",
      "tidyverse/lubridate"
    )
  ),
  clean = TRUE
)

message("Report rendered to ", file.path(output_dir, "index.html"))
