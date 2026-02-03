#' Validate repository vector format
#'
#' Internal function to validate that repository identifiers follow the expected
#' 'owner/repo' format.
#'
#' @param repos Character vector of repository identifiers
#' @return Invisible repos vector if valid, otherwise throws error
#' @keywords internal
validate_repo_vector <- function(repos) {
  if (!is.character(repos)) {
    rlang::abort("`repos` must be a character vector.")
  }
  if (!length(repos)) {
    rlang::abort("`repos` must contain at least one repository identifier.")
  }
  if (any(!nzchar(repos))) {
    rlang::abort("`repos` cannot contain empty strings.")
  }
  invalid <- !grepl("^[^/]+/[^/]+$", repos)
  if (any(invalid)) {
    rlang::abort(
      paste0(
        "All entries in `repos` must follow the 'owner/repo' format. Invalid: ",
        paste(repos[invalid], collapse = ", ")
      )
    )
  }
  invisible(repos)
}

#' Return first non-empty value from a list of candidates
#'
#' Internal utility function that returns the first non-NULL, non-empty string
#' from the provided arguments.
#'
#' @param ... Character values to evaluate
#' @return First non-empty value, or NULL if all are empty/NULL
#' @keywords internal
first_non_empty <- function(...) {
  for (candidate in list(...)) {
    if (!is.null(candidate) && nzchar(candidate)) {
      return(candidate)
    }
  }
  NULL
}

#' Null-coalescing operator
#'
#' Returns the right-hand side if left-hand side is NULL, otherwise returns
#' left-hand side.
#'
#' @param lhs Left-hand side value
#' @param rhs Right-hand side value (default if lhs is NULL)
#' @return lhs if not NULL, otherwise rhs
#' @keywords internal
#' @rdname or_pipe
#' @name or_pipe
`%||%` <- function(lhs, rhs) {
  if (is.null(lhs)) rhs else lhs
}

#' Sanitize issue count from GitHub API
#'
#' Internal function to safely convert GitHub API numeric fields to integers,
#' handling NULL, NA, and invalid values.
#'
#' @param x Value from GitHub API (could be NULL, NA, character, or numeric)
#' @return Integer count, defaulting to 0 for invalid values
#' @keywords internal
sanitize_issue_count <- function(x) {
  if (is.null(x)) {
    return(0)
  }
  if (length(x) == 0) {
    return(0)
  }
  if (is.na(x)) {
    return(0)
  }
  value <- suppressWarnings(as.numeric(x))
  if (is.na(value) || !is.finite(value)) {
    return(0)
  }
  value
}

#' Parse date for registry operations
#'
#' Internal function to safely parse date strings from qualification registry.
#'
#' @param x Character date string
#' @return Date object or NA if parsing fails
#' @keywords internal
parse_date_for_registry <- function(x) {
  if (is.null(x) || !nzchar(x)) {
    return(as.Date(NA))
  }
  suppressWarnings(as.Date(x))
}
