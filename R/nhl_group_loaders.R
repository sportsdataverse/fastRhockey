# Loaders for the season-by-season conference / division reference tables
# published by sportsdataverse/sdv-reference-data to the sportsdataverse-data
# release tag nhl_groups. Schema: that repo's CONTRACT.md. The tag carries
# parquet + csv; the csv is read with the contract's column classes (ids stay
# character, "150" not 150; an empty field is a null) so no parquet
# dependency is needed.

# Column classes of the four {league}_groups tables, per CONTRACT.md.
.groups_col_classes <- list(
  groups = c(
    league = "character", group_id = "character", level = "character",
    first_season = "integer", last_season = "integer", notes = "character"
  ),
  group_seasons = c(
    league = "character", group_id = "character", season = "integer",
    level = "character", name = "character", short_name = "character",
    abbreviation = "character", parent_group_id = "character",
    n_teams = "integer"
  ),
  group_aliases = c(
    league = "character", group_id = "character", source = "character",
    source_id = "character", name_kind = "character", value = "character",
    valid_from = "integer", valid_to = "integer"
  ),
  team_group_seasons = c(
    league = "character", season = "integer", team_id = "character",
    team_id_source = "character", team_name = "character",
    subdivision_id = "character", conference_id = "character",
    division_id = "character", source = "character",
    sources_agree = "logical", notes = "character"
  )
)

# Internal worker: validates seasons (per-season tables only), builds the
# release URLs, reads each csv with the contract column classes, optionally
# writes into a DB, and tags the result fastRhockey_data. A file that fails to
# download warns and contributes a zero-row frame carrying the contract schema.
.groups_release_loader <- function(league, table, description,
                                   seasons = NULL, min_season = NULL,
                                   max_season = NULL,
                                   dbConnection = NULL, tablename = NULL) {
  in_db <- !is.null(dbConnection) && !is.null(tablename)
  cols <- .groups_col_classes[[table]]

  # seasons = TRUE reads the release's all-seasons file
  file_stem <- paste0(league, "_", table)
  if (!is.null(min_season) && !isTRUE(seasons)) {
    stopifnot(is.numeric(seasons),
              all(seasons >= min_season),
              all(seasons <= max_season))
    file_stem <- paste0(file_stem, "_", seasons)
  }
  urls <- paste0(
    "https://github.com/sportsdataverse/sportsdataverse-data/releases/download/",
    league, "_groups/", file_stem, ".csv"
  )

  read_one <- function(url) {
    out <- data.table::as.data.table(lapply(cols, vector, length = 0))
    tryCatch(
      expr = {
        out <- csv_from_url(url, colClasses = cols, na.strings = "",
                            encoding = "UTF-8", showProgress = FALSE)
      },
      error = function(e) {
        cli::cli_warn("Failed to read {.url {url}}: {conditionMessage(e)}")
      }
    )
    out
  }

  p <- NULL
  if (is_installed("progressr")) p <- progressr::progressor(along = urls)
  out <- lapply(urls, progressively(read_one, p))
  out <- data.table::rbindlist(out, use.names = TRUE, fill = TRUE)
  if (in_db) {
    DBI::dbWriteTable(dbConnection, tablename, out, append = TRUE)
    return(invisible(NULL))
  }
  make_fastRhockey_data(out, description, Sys.time())
}

#' @title
#' **Load NHL groups (conferences and divisions) from the SportsDataverse data repo**
#' @rdname load_nhl_groups
#' @description One row per NHL group lineage (the league, its conferences
#'   and its divisions), keyed by the SportsDataverse group id (e.g.
#'   `nhl:metropolitan`). Published to the `nhl_groups` release tag on the
#'   [sportsdataverse-data releases](https://github.com/sportsdataverse/sportsdataverse-data/releases)
#'   by [sdv-reference-data](https://github.com/sportsdataverse/sdv-reference-data).
#'   Seasons are keyed by their *end year* (2025 = the 2024-25 season).
#' @param ... Additional arguments passed to an underlying function that
#'   writes the data into a database.
#' @param dbConnection A `DBIConnection` object, as returned by [DBI::dbConnect()]
#' @param tablename The name of the data table within the database
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name     |types     |description                                                                 |
#'    |:------------|:---------|:---------------------------------------------------------------------------|
#'    |league       |character |League key.                                                                 |
#'    |group_id     |character |SportsDataverse group id, `{league}:{slug}`; one id per lineage across renames. |
#'    |level        |character |Group level: `league`, `conference` or `division`.                         |
#'    |first_season |integer   |First season (end year) with at least one member.                           |
#'    |last_season  |integer   |Last season (end year) with at least one member.                            |
#'    |notes        |character |Lineage decisions and source caveats.                                       |
#'
#' @export
#' @examples
#' \donttest{
#'   try(load_nhl_groups())
#' }
load_nhl_groups <- function(..., dbConnection = NULL, tablename = NULL) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .groups_release_loader("nhl", "groups",
    "NHL groups from the SportsDataverse data repo",
    dbConnection = dbConnection, tablename = tablename)
}

#' @title
#' **Load NHL conference and division names and parents by season from the SportsDataverse data repo**
#' @rdname load_nhl_group_seasons
#' @description One row per NHL group per season it existed, with the name,
#'   abbreviation and parent group **as of that season** (not today's).
#'   Published to the `nhl_groups` release tag on the sportsdataverse-data
#'   releases. Seasons are keyed by their *end year* (2025 = 2024-25).
#' @inheritParams load_nhl_groups
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name        |types     |description                                                        |
#'    |:---------------|:---------|:------------------------------------------------------------------|
#'    |league          |character |League key.                                                        |
#'    |group_id        |character |SportsDataverse group id, `{league}:{slug}`.                       |
#'    |season          |integer   |Season (end year).                                                 |
#'    |level           |character |Group level: `league`, `conference` or `division`.                |
#'    |name            |character |Group name as of that season.                                       |
#'    |short_name      |character |Short name as of that season.                                      |
#'    |abbreviation    |character |Abbreviation as of that season.                                    |
#'    |parent_group_id |character |Parent group id as of that season (division, conference, league).  |
#'    |n_teams         |integer   |Member teams that season.                                          |
#'
#' @export
#' @examples
#' \donttest{
#'   try(load_nhl_group_seasons())
#' }
load_nhl_group_seasons <- function(..., dbConnection = NULL, tablename = NULL) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .groups_release_loader("nhl", "group_seasons",
    "NHL group seasons from the SportsDataverse data repo",
    dbConnection = dbConnection, tablename = tablename)
}

#' @title
#' **Load NHL group aliases from the SportsDataverse data repo**
#' @rdname load_nhl_group_aliases
#' @description Every name and id a source uses for an NHL conference or
#'   division, with the seasons it is valid for -- the crosswalk from NHL and
#'   ESPN ids and names to SportsDataverse group ids. Published to the
#'   `nhl_groups` release tag on the sportsdataverse-data releases.
#' @inheritParams load_nhl_groups
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name   |types     |description                                                                     |
#'    |:----------|:---------|:-------------------------------------------------------------------------------|
#'    |league     |character |League key.                                                                     |
#'    |group_id   |character |SportsDataverse group id, `{league}:{slug}`.                                    |
#'    |source     |character |Source that uses the alias (e.g. `nhl`, `espn`).                                |
#'    |source_id  |character |The source's own id for the group, when it has one.                             |
#'    |name_kind  |character |Alias kind: `name`, `short_name`, `abbreviation`, `slug` or `code`.            |
#'    |value      |character |The alias.                                                                      |
#'    |valid_from |integer   |First season (end year) the alias is valid (inclusive); `NA` = unbounded.       |
#'    |valid_to   |integer   |Last season (end year) the alias is valid (inclusive); `NA` = unbounded.        |
#'
#' @export
#' @examples
#' \donttest{
#'   try(load_nhl_group_aliases())
#' }
load_nhl_group_aliases <- function(..., dbConnection = NULL, tablename = NULL) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .groups_release_loader("nhl", "group_aliases",
    "NHL group aliases from the SportsDataverse data repo",
    dbConnection = dbConnection, tablename = tablename)
}

#' @title
#' **Load NHL team conference and division memberships by season from the SportsDataverse data repo**
#' @rdname load_nhl_team_group_seasons
#' @description One row per NHL team per season, with the conference and
#'   division it played in that season, e.g. the Detroit Red Wings moving to
#'   `nhl:atlantic` in the 2014 (2013-14) realignment. Published to the
#'   `nhl_groups` release tag on the sportsdataverse-data releases, one file
#'   per season.
#' @param seasons A vector of 4-digit years (the *end year* of the NHL
#'   season; e.g., 2026 for the 2025-26 season), or `TRUE` for every
#'   published season. Min: 1918. There is no 2005 (the 2004-05 lockout).
#' @inheritParams load_nhl_groups
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name       |types     |description                                                                  |
#'    |:--------------|:---------|:----------------------------------------------------------------------------|
#'    |league         |character |League key.                                                                  |
#'    |season         |integer   |Season (end year).                                                           |
#'    |team_id        |character |ESPN team id where ESPN covers the team, otherwise the NHL team id.          |
#'    |team_id_source |character |Id system of `team_id`: `espn` or `nhl`.                                     |
#'    |team_name      |character |Team name as of that season.                                                 |
#'    |subdivision_id |character |SportsDataverse subdivision group id; `NA` for the NHL.                      |
#'    |conference_id  |character |SportsDataverse conference group id; `NA` where the level does not apply.    |
#'    |division_id    |character |SportsDataverse division group id; `NA` where the level does not apply.      |
#'    |source         |character |Source the membership came from.                                             |
#'    |sources_agree  |logical   |Whether a second source agrees; `NA` when only one source covers the season. |
#'    |notes          |character |Notes, e.g. the NHL's own team id.                                           |
#'
#' @export
#' @examples
#' \donttest{
#'   try(load_nhl_team_group_seasons(seasons = 2014))
#' }
load_nhl_team_group_seasons <- function(seasons = most_recent_nhl_season(), ...,
                                        dbConnection = NULL, tablename = NULL) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .groups_release_loader("nhl", "team_group_seasons",
    "NHL team group seasons from the SportsDataverse data repo",
    seasons = seasons, min_season = 1918,
    max_season = most_recent_nhl_season(),
    dbConnection = dbConnection, tablename = tablename)
}
