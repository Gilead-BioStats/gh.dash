#' Get default qualification registry URL
#'
#' Returns the default URL for the qualification registry, checking options and
#' environment variables for overrides.
#'
#' @return Character string with registry URL
#' @keywords internal
default_qualification_registry_url <- function() {
  getOption(
    "gh.dash.qualification_registry_url",
    Sys.getenv(
      "GH_DASH_QUAL_REGISTRY_URL",
      ""
    )
  )
}

#' Fetch qualification registry from URL
#'
#' Downloads and processes the qualification registry CSV, with fallback to
#' GitHub API if direct CSV access fails.
#'
#' @param url URL to the qualification registry CSV
#' @param token GitHub personal access token (optional)
#' @return Data frame with qualification registry or NULL if fetch fails
#' @export
fetch_qualification_registry <- function(
  url = default_qualification_registry_url(),
  token = NULL
) {
  if (is.null(url) || !nzchar(url)) {
    return(NULL)
  }

  df <- try_read_registry_csv(url)

  if (is.null(df)) {
    df <- try_fetch_registry_via_github_api(url, token)
  }

  normalize_qualification_registry(df)
}

#' Try to read registry CSV directly
#'
#' Internal function to attempt direct CSV reading from URL.
#'
#' @param url URL to CSV file
#' @return Data frame or NULL if read fails
#' @keywords internal
try_read_registry_csv <- function(url) {
  tryCatch(
    suppressWarnings(utils::read.csv(url, stringsAsFactors = FALSE)),
    error = function(...) NULL
  )
}

#' Try to fetch registry via GitHub API
#'
#' Internal function to fetch registry content through GitHub API when direct
#' CSV access fails.
#'
#' @param url GitHub file URL
#' @param token GitHub personal access token (optional)
#' @return Data frame or NULL if fetch fails
#' @keywords internal
try_fetch_registry_via_github_api <- function(url, token) {
  spec <- parse_github_file_url(url)

  if (is.null(spec)) {
    return(NULL)
  }

  content <- safe_gh(
    gh::gh,
    "GET /repos/{owner}/{repo}/contents/{path}",
    owner = spec$owner,
    repo = spec$repo,
    path = spec$path,
    ref = spec$ref,
    .token = token
  )

  if (is.null(content) || is.null(content$content)) {
    return(NULL)
  }

  decoded <- decode_base64_string(content$content)
  if (is.null(decoded)) {
    return(NULL)
  }

  tryCatch(
    utils::read.csv(text = decoded, stringsAsFactors = FALSE),
    error = function(...) NULL
  )
}

#' Parse GitHub file URL
#'
#' Internal function to parse GitHub raw or blob URLs into components.
#'
#' @param url GitHub file URL
#' @return List with owner, repo, ref, and path components or NULL if parse fails
#' @keywords internal
parse_github_file_url <- function(url) {
  raw_pattern <- "^https?://raw.githubusercontent.com/([^/]+)/([^/]+)/([^/]+)/(.+)$"
  blob_pattern <- "^https?://github.com/([^/]+)/([^/]+)/blob/([^/]+)/(.+)$"

  raw_match <- regexec(raw_pattern, url)
  raw_parts <- regmatches(url, raw_match)[[1]]
  if (length(raw_parts)) {
    return(list(owner = raw_parts[[2]], repo = raw_parts[[3]], ref = raw_parts[[4]], path = raw_parts[[5]]))
  }

  blob_match <- regexec(blob_pattern, url)
  blob_parts <- regmatches(url, blob_match)[[1]]
  if (length(blob_parts)) {
    return(list(owner = blob_parts[[2]], repo = blob_parts[[3]], ref = blob_parts[[4]], path = blob_parts[[5]]))
  }

  NULL
}

#' Decode base64 string
#'
#' Internal function to decode base64 content from GitHub API.
#'
#' @param x Base64 encoded string
#' @return Decoded character string or NULL if decode fails
#' @keywords internal
decode_base64_string <- function(x) {
  if (!requireNamespace("base64enc", quietly = TRUE)) {
    return(NULL)
  }

  tryCatch(
    rawToChar(base64enc::base64decode(x)),
    error = function(...) NULL
  )
}

#' Normalize qualification registry data frame
#'
#' Internal function to validate and normalize the qualification registry
#' data structure.
#'
#' @param df Data frame from qualification registry
#' @return Normalized data frame or NULL if invalid
#' @keywords internal
normalize_qualification_registry <- function(df) {
  if (is.null(df) || !nrow(df)) {
    return(NULL)
  }

  required <- c(
    "org",
    "repo",
    "version",
    "release.url",
    "release.date",
    "qualification.url",
    "qualification.date"
  )

  if (!all(required %in% names(df))) {
    return(NULL)
  }

  df
}

#' Lookup qualification entry for repository and version
#'
#' Internal function to find qualification registry entry matching the
#' repository and release tag.
#'
#' @param owner Repository owner
#' @param repo Repository name
#' @param tag Release tag
#' @param registry Qualification registry data frame
#' @return List with qualification details or NULL if not found
#' @keywords internal
lookup_qualification_entry <- function(owner, repo, tag, registry) {
  if (is.null(registry) || !nrow(registry)) {
    return(NULL)
  }

  candidates <- unique(c(tag, sub("^v", "", tag)))
  matches <- registry$org == owner & registry$repo == repo & registry$version %in% candidates

  if (!any(matches, na.rm = TRUE)) {
    return(NULL)
  }

  row <- registry[which(matches)[1], , drop = FALSE]

  qualification_url <- first_non_empty(row$qualification.url, row$release.url)
  qualification_date <- first_non_empty(row$qualification.date, row$release.date)

  if (is.null(qualification_url) || !nzchar(qualification_url)) {
    return(NULL)
  }

  list(
    qualification_url = qualification_url,
    qualification_date = qualification_date
  )
}

#' Lookup prior qualification entry
#'
#' Internal function to find the most recent prior qualification for a
#' repository when current version is not qualified.
#'
#' @param owner Repository owner
#' @param repo Repository name
#' @param tag Current release tag
#' @param registry Qualification registry data frame
#' @return List with prior qualification details or NULL if not found
#' @keywords internal
lookup_prior_qualification_entry <- function(owner, repo, tag, registry) {
  if (is.null(registry) || !nrow(registry)) {
    return(NULL)
  }

  candidates <- unique(c(tag, sub("^v", "", tag)))
  rows <- registry[registry$org == owner & registry$repo == repo & !(registry$version %in% candidates), , drop = FALSE]

  if (!nrow(rows)) {
    return(NULL)
  }

  dates <- vapply(seq_len(nrow(rows)), function(i) {
    parse_date_for_registry(rows$qualification.date[[i]] %||% rows$release.date[[i]])
  }, as.Date(NA))

  # Pick the most recent dated entry; if all NA, pick the first.
  if (all(is.na(dates))) {
    idx <- 1L
  } else {
    idx <- order(dates, decreasing = TRUE, na.last = TRUE)[1]
  }

  row <- rows[idx, , drop = FALSE]

  qualification_url <- first_non_empty(row$qualification.url, row$release.url)

  if (is.null(qualification_url) || !nzchar(qualification_url)) {
    return(NULL)
  }

  list(
    qualification_url = qualification_url,
    qualification_date = row$qualification.date %||% row$release.date,
    version = row$version
  )
}
