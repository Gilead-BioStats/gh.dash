#' Format repository link as HTML
#'
#' Internal function to create an HTML link for a GitHub repository.
#' Includes a lock icon if the repository is private.
#'
#' @param owner Repository owner (GitHub username or organization)
#' @param repo Repository name
#' @param is_private Logical indicating if repository is private
#' @return Character string with HTML anchor tag
#' @keywords internal
#' @importFrom htmltools tags
format_repo_link <- function(owner, repo, is_private = FALSE) {
  url <- sprintf("https://github.com/%s/%s", owner, repo)
  repo_text <- sprintf("%s/%s", owner, repo)

  if (isTRUE(is_private)) {
    lock_icon <- htmltools::tags$span(
      title = "Private repository",
      "\U0001F512 "
    )
    link_content <- htmltools::tagList(lock_icon, repo_text)
  } else {
    link_content <- repo_text
  }

  as.character(htmltools::tags$a(href = url, link_content))
}

#' Format release summary with qualification badge
#'
#' Internal function to format GitHub release information as styled HTML,
#' including qualification status if available.
#'
#' @param owner Repository owner (GitHub username or organization)
#' @param repo Repository name
#' @param release Release object from GitHub API
#' @param registry Qualification registry data frame
#' @return Character string with formatted HTML
#' @keywords internal
#' @importFrom htmltools tags HTML htmlEscape
format_release_summary <- function(owner, repo, release, registry) {
  base_url <- sprintf("https://github.com/%s/%s", owner, repo)
  releases_url <- paste0(base_url, "/releases")

  if (is.null(release)) {
    label <- tailwind_label(
      "No release",
      title = "No published release found",
      variant = "slate"
    )
    link <- releases_url
    return(as.character(htmltools::tags$a(href = link, htmltools::HTML(label))))
  }

  tag <- first_non_empty(release$tag_name, release$name, "Unnamed release")
  date <- release$published_at

  if (!is.null(date) && nzchar(date)) {
    pretty <- substr(date, 1L, 10L)
    label <- tailwind_label(
      tag,
      title = paste0("Released ", pretty),
      variant = "emerald"
    )
  } else {
    label <- tailwind_label(tag, title = "Release date unavailable", variant = "emerald")
  }

  target <- first_non_empty(release$html_url, paste0(releases_url, "/tag/", tag), releases_url)
  release_badge <- as.character(htmltools::tags$a(href = target, htmltools::HTML(label)))

  qualification_badge <- format_release_qualification(owner, repo, tag, registry)
  badges <- c(release_badge, qualification_badge)
  paste(badges[nzchar(badges)], collapse = " ")
}

#' Format release qualification badge
#'
#' Internal function to create qualification status badge for a release.
#'
#' @param owner Repository owner
#' @param repo Repository name
#' @param tag Release tag
#' @param registry Qualification registry data frame
#' @return Character string with qualification badge HTML or empty string
#' @keywords internal
format_release_qualification <- function(owner, repo, tag, registry) {
  entry <- lookup_qualification_entry(owner, repo, tag, registry)

  if (!is.null(entry)) {
    return(build_qualification_badge(
      url = entry$qualification_url,
      title = build_qualification_title(entry$qualification_date),
      variant = "emerald"
    ))
  }

  prior <- lookup_prior_qualification_entry(owner, repo, tag, registry)

  if (!is.null(prior)) {
    return(build_qualification_badge(
      url = prior$qualification_url,
      title = build_prior_qualification_title(prior$qualification_date, prior$version),
      variant = "slate"
    ))
  }

  ""
}

#' Build qualification badge HTML
#'
#' Internal function to create the HTML for qualification badges.
#'
#' @param url Link URL for the badge
#' @param title Tooltip text
#' @param variant Color variant ("emerald" or "slate")
#' @return Character string with badge HTML
#' @keywords internal
#' @importFrom htmltools tags HTML htmlEscape
build_qualification_badge <- function(url, title, variant = c("emerald", "slate")) {
  variant <- match.arg(variant)

  label <- sprintf(
    '<span class="badge badge--%s" title="%s">&#128737;</span>',
    variant,
    htmltools::htmlEscape(title)
  )

  as.character(
    htmltools::tags$a(
      href = url,
      htmltools::HTML(label)
    )
  )
}

#' Build qualification title text
#'
#' Internal function to create qualification badge tooltip text.
#'
#' @param date Qualification date
#' @return Character string for tooltip
#' @keywords internal
build_qualification_title <- function(date) {
  if (is.null(date) || !nzchar(date)) {
    return("Package qualified")
  }

  paste0("Package qualified on ", date)
}

#' Build prior qualification title text
#'
#' Internal function to create prior qualification badge tooltip text.
#'
#' @param date Prior qualification date
#' @param version Prior qualified version
#' @return Character string for tooltip
#' @keywords internal
build_prior_qualification_title <- function(date, version) {
  base <- sprintf("Previously qualified (version %s)", version %||% "unknown")
  if (is.null(date) || !nzchar(date)) {
    return(base)
  }
  paste0(base, " on ", date)
}

#' Format milestone summary as HTML
#'
#' Internal function to format GitHub milestones as styled HTML.
#'
#' @param owner Repository owner (GitHub username or organization)
#' @param repo Repository name
#' @param milestones List of milestone objects from GitHub API
#' @return Character string with formatted HTML
#' @keywords internal
#' @importFrom htmltools tags HTML
format_milestone_summary <- function(owner, repo, milestones) {
  base_url <- sprintf("https://github.com/%s/%s/milestones", owner, repo)

  if (is.null(milestones) || !length(milestones)) {
    label <- tailwind_label("None", title = "No open milestones", variant = "slate")
    return(as.character(htmltools::tags$a(href = base_url, htmltools::HTML(label))))
  }

  entries <- vapply(
    milestones,
    format_single_milestone,
    character(1),
    owner = owner,
    repo = repo,
    USE.NAMES = FALSE
  )
  entries <- entries[nzchar(entries)]

  if (!length(entries)) {
    label <- tailwind_label("None", title = "No open milestones", variant = "slate")
    return(as.character(htmltools::tags$a(href = base_url, htmltools::HTML(label))))
  }

  paste(entries, collapse = " ")
}

#' Format single milestone as HTML
#'
#' Internal function to format a single GitHub milestone with progress indicator.
#'
#' @param milestone Milestone object from GitHub API
#' @param owner Repository owner (GitHub username or organization)
#' @param repo Repository name
#' @return Character string with formatted HTML
#' @keywords internal
#' @importFrom htmltools tags HTML
format_single_milestone <- function(milestone, owner, repo) {
  open_count <- sanitize_issue_count(milestone$open_issues)
  closed_count <- sanitize_issue_count(milestone$closed_issues)
  total <- open_count + closed_count

  if (!is.finite(total) || total == 0) {
    return("")
  }

  title <- first_non_empty(milestone$title, "Unnamed milestone")
  completion <- closed_count / total

  label <- grayscale_milestone_label(
    title,
    tooltip = sprintf("%s: %s open of %s", title, open_count, total),
    completion = completion
  )

  target <- first_non_empty(
    milestone$html_url,
    sprintf("https://github.com/%s/%s/milestone/%s", owner, repo, sanitize_issue_count(milestone$number)),
    sprintf("https://github.com/%s/%s/milestones", owner, repo)
  )

  as.character(htmltools::tags$a(href = target, htmltools::HTML(label)))
}

#' Create Tailwind CSS label
#'
#' Internal function to create styled HTML labels using Tailwind CSS classes.
#'
#' @param text Label text
#' @param title Tooltip text
#' @param variant Color variant ("sky", "emerald", or "slate")
#' @return Character string with HTML span element
#' @keywords internal
#' @importFrom htmltools htmlEscape
tailwind_label <- function(text, title, variant = c("sky", "emerald", "slate")) {
  variant <- match.arg(variant)
  classes <- list(
    sky = "badge badge--sky",
    emerald = "badge badge--emerald",
    slate = "badge badge--slate"
  )

  sprintf(
    '<span class="%s" title="%s">%s</span>',
    classes[[variant]],
    htmltools::htmlEscape(title),
    htmltools::htmlEscape(text)
  )
}

#' Create grayscale milestone label with progress
#'
#' Internal function to create milestone labels with visual progress indicators.
#'
#' @param text Label text
#' @param tooltip Tooltip text
#' @param completion Numeric completion ratio (0-1)
#' @return Character string with styled HTML span element
#' @keywords internal
#' @importFrom htmltools htmlEscape
grayscale_milestone_label <- function(text, tooltip, completion) {
  completion <- if (is.finite(completion)) max(0, min(1, completion)) else 0
  fill_color <- "#38bdf8"
  remainder_color <- "#e0f2fe"
  fill_percent <- round(completion * 100, 1)
  meter_background <- sprintf(
    "linear-gradient(90deg, %1$s 0%%, %1$s %2$.1f%%, %3$s %2$.1f%%, %3$s 100%%)",
    fill_color,
    fill_percent,
    remainder_color
  )

  sprintf(
    '<span class="badge badge--milestone" title="%s" style="background:%s;">%s</span>',
    htmltools::htmlEscape(tooltip),
    meter_background,
    htmltools::htmlEscape(text)
  )
}

#' Format issue summary as HTML
#'
#' Internal function to format GitHub issue counts as a hyperlink pointing to
#' the repository's issues page. The link text shows the number of issues opened
#' and closed in the past 365 days. The tooltip shows the number of issues
#' opened and closed in the past 90 days. When counts are \code{NA_integer_}
#' the cell shows "Unavailable" and the tooltip gives a failure-specific reason
#' sourced from the \code{reason} element of the input lists.
#'
#' @param owner Repository owner (GitHub username or organization)
#' @param repo Repository name
#' @param issues_365day Named list with integer elements \code{opened} and
#'   \code{closed} for the past 365 days, as returned by
#'   \code{fetch_issue_counts()}. Elements may be \code{NA_integer_} on
#'   failure; in that case an optional \code{reason} character element
#'   (\code{"rate_limited"} or \code{"unavailable"}) controls the tooltip text.
#' @param issues_90day Named list with integer elements \code{opened} and
#'   \code{closed} for the past 90 days, as returned by
#'   \code{fetch_issue_counts()}. Elements may be \code{NA_integer_} on
#'   failure; in that case an optional \code{reason} character element
#'   (\code{"rate_limited"} or \code{"unavailable"}) controls the tooltip text.
#' @return Character string with HTML anchor tag
#' @keywords internal
#' @importFrom htmltools tags
format_issue_summary <- function(owner, repo, issues_365day, issues_90day) {
  url <- sprintf("https://github.com/%s/%s/issues", owner, repo)
  opened_365 <- issues_365day$opened
  closed_365 <- issues_365day$closed
  opened_90  <- issues_90day$opened
  closed_90  <- issues_90day$closed
  link_text <- if (is.na(opened_365) || is.na(closed_365)) {
    "Unavailable"
  } else {
    sprintf("%d / %d", opened_365, closed_365)
  }
  tooltip <- if (is.na(opened_90) || is.na(closed_90)) {
    if (identical(issues_90day$reason, "rate_limited")) {
      "Issue data unavailable (rate limit reached)"
    } else {
      "Issue data unavailable"
    }
  } else {
    sprintf("%d opened / %d closed in past 90 days", opened_90, closed_90)
  }
  as.character(htmltools::tags$a(href = url, title = tooltip, link_text))
}

#' Format pull request summary as HTML
#'
#' Internal function to format GitHub pull requests count with link as HTML.
#'
#' @param owner Repository owner (GitHub username or organization)
#' @param repo Repository name
#' @param pr_count Integer count of open pull requests
#' @return Character string with formatted HTML link
#' @keywords internal
#' @importFrom htmltools tags
format_pr_summary <- function(owner, repo, pr_count) {
  base_url <- sprintf("https://github.com/%s/%s/pulls", owner, repo)

  if (pr_count == 0) {
    return(as.character(htmltools::tags$a(href = base_url, "None")))
  }

  as.character(
    htmltools::tags$a(
      href = base_url,
      sprintf("%d", pr_count)
    )
  )
}

#' Format branch comparison as HTML
#'
#' Internal function to format branch comparison results as HTML.
#'
#' @param owner Repository owner (GitHub username or organization)
#' @param repo Repository name
#' @param comparison Comparison object from GitHub API
#' @return Character string with HTML anchor tag
#' @keywords internal
#' @importFrom htmltools tags
format_branch_comparison <- function(owner, repo, comparison) {
  status_text <- branch_status_text(comparison)

  repo_url <- sprintf("https://github.com/%s/%s/compare/main...dev", owner, repo)
  anchor <- htmltools::tags$a(href = repo_url, status_text)
  as.character(anchor)
}

#' Parse release published date
#'
#' Internal function to parse the `published_at` field of a GitHub release
#' object into a `Date`.
#'
#' @param published_at ISO 8601 date-time string from GitHub API (e.g.
#'   `"2025-01-15T12:00:00Z"`)
#' @return A `Date` value, or `NA` if the input is `NULL`, empty, or
#'   unparseable.
#' @keywords internal
parse_release_date <- function(published_at) {
  if (is.null(published_at) || length(published_at) == 0L) {
    return(as.Date(NA_character_))
  }
  published_at <- as.character(published_at)
  if (!nzchar(published_at)) {
    return(as.Date(NA_character_))
  }
  tryCatch(
    as.Date(substr(published_at, 1L, 10L)),
    error = function(e) as.Date(NA_character_)
  )
}

#' Derive latest published release from a releases list
#'
#' Internal function to find the latest non-draft, non-prerelease entry
#' from the list returned by `fetch_releases()`.
#'
#' @param releases List of release objects from `fetch_releases()`
#' @return A single release list element, or `NULL` if none qualify.
#' @keywords internal
#' @importFrom purrr detect
derive_latest_release <- function(releases) {
  if (is.null(releases) || !length(releases)) {
    return(NULL)
  }
  purrr::detect(releases, ~ !isTRUE(.x$draft) && !isTRUE(.x$prerelease))
}

#' Count releases from last N days
#'
#' Internal function to count the number of non-draft, non-prerelease
#' releases published in the last N days (inclusive).
#'
#' @param releases List of release objects from `fetch_releases()`
#' @param days Number of days to look back from today
#' @return Integer count of qualifying releases.
#' @keywords internal
#' @importFrom purrr keep
count_releases_last_n_days <- function(releases, days) {
  if (is.null(releases) || !length(releases)) {
    return(0L)
  }
  today <- Sys.Date()
  start_date <- today - days

  releases |>
    purrr::keep(~ !isTRUE(.x$draft) && !isTRUE(.x$prerelease)) |>
    purrr::keep(~ {
      if (is.null(.x$published_at)) {
        return(FALSE)
      }
      pub_date <- parse_release_date(.x$published_at)
      !is.na(pub_date) && pub_date >= start_date && pub_date <= today
    }) |>
    length() |>
    as.integer()
}

#' Count releases from last 365 days
#'
#' Internal function to count the number of non-draft, non-prerelease
#' releases published in the last 365 days (inclusive).
#'
#' @param releases List of release objects from `fetch_releases()`
#' @return Integer count of qualifying releases.
#' @keywords internal
count_ytd_releases <- function(releases) {
  count_releases_last_n_days(releases, 365)
}

#' Format release count from last 365 days as HTML
#'
#' Internal function to format the release count from the last 365 days
#' as a hyperlink pointing to the repository's releases page. Tooltip shows
#' the count for the last 90 days.
#'
#' @param owner Repository owner (GitHub username or organization)
#' @param repo Repository name
#' @param releases List of release objects from `fetch_releases()`
#' @return Character string with HTML anchor tag
#' @keywords internal
#' @importFrom htmltools tags
format_ytd_releases <- function(owner, repo, releases) {
  count_365 <- count_ytd_releases(releases)
  count_90 <- count_releases_last_n_days(releases, 90)
  url <- sprintf("https://github.com/%s/%s/releases", owner, repo)
  tooltip <- sprintf("%s releases in past 90 days", count_90)
  as.character(htmltools::tags$a(href = url, title = tooltip, as.character(count_365)))
}

#' Get branch status text
#'
#' Internal function to convert GitHub branch comparison into readable status text.
#'
#' @param comparison Comparison object from GitHub API
#' @return Character string describing branch status
#' @keywords internal
branch_status_text <- function(comparison) {
  if (is.null(comparison)) {
    return("Unavailable")
  }

  ahead <- sanitize_issue_count(comparison$ahead_by)
  behind <- sanitize_issue_count(comparison$behind_by)

  if (ahead == 0 && behind == 0) {
    return("In sync")
  }

  if (ahead > 0 && behind == 0) {
    return(sprintf("+%s", ahead))
  }

  if (behind > 0 && ahead == 0) {
    return(sprintf("-%s", behind))
  }

  if (ahead > 0 && behind > 0) {
    return(sprintf("+%s, -%s", ahead, behind))
  }

  "Unavailable"
}
