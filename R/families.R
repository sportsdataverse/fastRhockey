# Family index pages.
#
# roxygen's `@family` writes every member of a family into the "See also" of
# every other member, so a family of n pages carries n^2 links. For the large
# families that was most of the installed help (ESPN NHL and ESPN MCH alone:
# about 1.9 MB of Rd). Their members carry `@concept <family>` and one
# `@seealso` link to the family's index page below instead; the index lists
# the family once, built at document() time from the `@concept` tags in R/.
# Families of 14 or fewer keep `@family`.

#' Roxygen lines listing every function whose roxygen block carries
#' `@concept <family>` (roxygen `@eval`, run by `devtools::document()` from the
#' package root).
#'
#' A function counts when its block is followed by its definition
#' (`name <- function`), whether it has a page of its own or shares one through
#' `@rdname`.
#'
#' @param family The family name, exactly as in the `@concept` tags.
#' @param note Optional extra paragraph for the index page.
#' @return Character vector of roxygen lines.
#' @noRd
.family_index <- function(family, note = NULL) {
  fns <- character()
  for (path in sort(list.files("R", "[.][Rr]$", full.names = TRUE))) {
    l <- readLines(path, warn = FALSE)
    for (i in which(l == paste0("#' @concept ", family))) {
      j <- i
      while (j <= length(l) && startsWith(l[j], "#'")) j <- j + 1L
      m <- regmatches(l[j], regexec("^`?([A-Za-z][A-Za-z0-9._]*)`? *<- *function", l[j]))[[1]]
      if (length(m)) fns <- c(fns, m[2])
    }
  }
  fns <- sort(unique(fns))
  if (!length(fns)) stop("no function carries `@concept ", family, "`", call. = FALSE)
  c("@description",
    paste0("Every function in the ", family, " family (", length(fns), "):"),
    "",
    paste0("* [", fns, "()]"),
    if (length(note)) c("", note))
}

#' ESPN NHL Functions: family index
#'
#' @name family_espn_nhl
#' @eval .family_index("ESPN NHL Functions")
#' @keywords internal
NULL

#' ESPN MCH / WCH Functions: family index
#'
#' @name family_espn_mch
#' @eval .family_index("ESPN MCH Functions", "Each `espn_wch_*()` function shares the help page of its `espn_mch_*()` twin.")
#' @keywords internal
NULL

#' SIHF Functions: family index
#'
#' @name family_sihf
#' @eval .family_index("SIHF Functions")
#' @keywords internal
NULL

#' SHL Functions: family index
#'
#' @name family_shl
#' @eval .family_index("SHL Functions")
#' @keywords internal
NULL

#' KHL Functions: family index
#'
#' @name family_khl
#' @eval .family_index("KHL Functions")
#' @keywords internal
NULL

#' Liiga Functions: family index
#'
#' @name family_liiga
#' @eval .family_index("Liiga Functions")
#' @keywords internal
NULL

#' National League Functions: family index
#'
#' @name family_national_league
#' @eval .family_index("National League Functions")
#' @keywords internal
NULL
