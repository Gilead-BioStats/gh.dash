#' Format repository link as HTML
#'
#' Internal function to create an HTML link for a GitHub repository.
#'
#' @param owner Repository owner (GitHub username or organization)
#' @param repo Repository name
#' @return Character string with HTML anchor tag
#' @keywords internal
#' @importFrom htmltools tags
format_repo_link <- function(owner, repo) {
  url <- sprintf("https://github.com/%s/%s", owner, repo)
  as.character(htmltools::tags$a(href = url, sprintf("%s/%s", owner, repo)))
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

#' Format open pull requests summary
#'
#' Internal function to format open pull requests as HTML badges.
#'
#' @param owner Repository owner (GitHub username or organization)
#' @param repo Repository name
#' @param prs List of pull request objects from GitHub API
#' @return Character string with formatted HTML
#' @keywords internal
#' @importFrom htmltools tags HTML htmlEscape
format_pr_summary <- function(owner, repo, prs) {
  base_url <- sprintf("https://github.com/%s/%s/pulls", owner, repo)

  if (is.null(prs) || !length(prs)) {
    label <- tailwind_label("None", title = "No open pull requests", variant = "slate")
    return(as.character(htmltools::tags$a(href = base_url, htmltools::HTML(label))))
  }

  entries <- vapply(
    prs,
    format_single_pr,
    character(1),
    owner = owner,
    repo = repo,
    USE.NAMES = FALSE
  )
  entries <- entries[nzchar(entries)]

  if (!length(entries)) {
    label <- tailwind_label("None", title = "No open pull requests", variant = "slate")
    return(as.character(htmltools::tags$a(href = base_url, htmltools::HTML(label))))
  }

  paste(entries, collapse = " ")
}

#' Format single pull request as HTML badge
#'
#' Internal function to format a single GitHub pull request as a badge with metadata.
#'
#' @param pr Pull request object from GitHub API
#' @param owner Repository owner (GitHub username or organization)
#' @param repo Repository name
#' @return Character string with formatted HTML
#' @keywords internal
#' @importFrom htmltools tags HTML htmlEscape
format_single_pr <- function(pr, owner, repo) {
  title <- first_non_empty(pr$title, "Untitled PR")
  number <- sanitize_issue_count(pr$number)
  
  if (!is.finite(number) || number == 0) {
    return("")
  }

  # Build tooltip with PR information
  tooltip_parts <- c(paste0("PR #", number, ": ", title))
  
  # Add commits info if available
  if (!is.null(pr$commits) && is.finite(pr$commits)) {
    commits_text <- if (pr$commits == 1) "1 commit" else paste(pr$commits, "commits")
    tooltip_parts <- c(tooltip_parts, commits_text)
  }
  
  # Add linked issues count if available (approximated by counting "closes #" patterns)
  if (!is.null(pr$body) && nzchar(pr$body)) {
    closes_pattern <- gregexpr("(?i)(close[sd]?|fix(e[sd])?|resolve[sd]?)\\s*#\\d+", pr$body, perl = TRUE)
    if (length(closes_pattern[[1]]) > 0 && closes_pattern[[1]][1] != -1) {
      issues_count <- length(closes_pattern[[1]])
      issues_text <- if (issues_count == 1) "closes 1 issue" else paste("closes", issues_count, "issues")
      tooltip_parts <- c(tooltip_parts, issues_text)
    }
  }
  
  tooltip <- paste(tooltip_parts, collapse = "; ")

  # Create the badge - using "emerald" variant for PRs
  pr_label <- paste0("#", number)
  label <- tailwind_label(
    pr_label,
    title = tooltip,
    variant = "emerald"
  )

  # Link to the specific PR
  target <- first_non_empty(
    pr$html_url,
    sprintf("https://github.com/%s/%s/pull/%s", owner, repo, number)
  )

  as.character(htmltools::tags$a(href = target, htmltools::HTML(label)))
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
