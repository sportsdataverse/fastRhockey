#' @title **NHL Game Shifts**
#' @description Returns information on game shifts for a given game id
#' @param game_id Game unique ID
#' @return A data frame (`fastRhockey_data`) with one row per shift change and
#' the following columns:
#'
#'    |col_name               |types     |description                              |
#'    |:----------------------|:---------|:----------------------------------------|
#'    |event_team             |character |Team making the line change.             |
#'    |period                 |integer   |Period number.                           |
#'    |period_time            |character |Time in the period of the change (MM:SS).|
#'    |period_seconds         |numeric   |Seconds elapsed in the period.           |
#'    |game_seconds           |numeric   |Seconds elapsed in the game.             |
#'    |num_on                 |integer   |Number of players coming on the ice.     |
#'    |players_on             |character |Names of players coming on the ice.      |
#'    |ids_on                 |character |Player IDs coming on the ice.            |
#'    |num_off                |integer   |Number of players going off the ice.     |
#'    |players_off            |character |Names of players going off the ice.      |
#'    |ids_off                |character |Player IDs going off the ice.            |
#'    |event                  |character |Event label ("Change").                  |
#'    |event_type             |character |Event type code ("CHANGE").              |
#'    |game_seconds_remaining |numeric   |Seconds remaining in the game.           |
#'
#' Source of truth is the legacy stats-API shiftcharts endpoint
#' (`api.nhle.com/stats/rest/en/shiftcharts`). When that endpoint returns
#' `{total: 0, data: []}` -- which has become common for 2024-25 and 2025-26
#' regular-season games -- we fall back to scraping the legacy HTML TOI
#' reports at `nhl.com/scores/htmlreports/{season}/T{H|V}{gameno}.HTM`,
#' which still publish per-shift records for the same games.
#' @keywords NHL Game Shifts
#' @import rvest
#' @importFrom rlang .data
#' @importFrom lubridate ms period_to_seconds
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr mutate filter select rename bind_cols bind_rows
#' @importFrom tidyr unnest unnest_wider everything
#' @importFrom janitor clean_names
#' @export
#' @examples
#' \donttest{
#'   try(nhl_game_shifts(game_id = 2021020182))
#' }
nhl_game_shifts <- function(game_id){

  base_url <- "https://api.nhle.com/stats/rest/en/shiftcharts?cayenneExp=gameId="

  full_url <- paste0(base_url,
                     game_id)
  res <- .retry_request(full_url)

  # Check the result
  check_status(res)

  # Default return so the outer `return(shifts)` cannot error with
  # "object 'shifts' not found" if the body below short-circuits or errors.
  shifts <- NULL

  tryCatch(
    expr = {
      resp <- .resp_text(res)
      site <- jsonlite::fromJSON(resp)

      # The endpoint is no longer populated for a growing share of
      # 2024-25 and 2025-26 games (~38% of 2025-26 as of late 2026).
      # When the API returns `{total: 0, data: []}`, fall back to the
      # legacy HTML TOI reports before declaring the game data-less.
      data_empty <- is.null(site$data) ||
        (is.data.frame(site$data) && nrow(site$data) == 0) ||
        (is.list(site$data) && length(site$data) == 0)

      if (data_empty) {
        shifts_raw <- .parse_toi_html(game_id)
      } else {
        shifts_raw <- site$data %>%
          dplyr::tibble() %>%
          janitor::clean_names() %>%
          tidyr::unite("player_name", c("first_name", "last_name"), sep = " ") %>%
          dplyr::select("game_id", "player_id", "player_name", "team_abbrev", "team_id",
                        "team_name", "period", "start_time", "end_time", "duration") %>%
          dplyr::filter(!is.na(.data$duration)) %>%
          dplyr::mutate(
            start_time_ms = lubridate::ms(.data$start_time),
            start_seconds = lubridate::period_to_seconds(.data$start_time_ms),
            start_game_seconds = .data$start_seconds + (1200 * (.data$period-1)),
            end_time_ms = lubridate::ms(.data$end_time),
            end_seconds = lubridate::period_to_seconds(.data$end_time_ms),
            end_game_seconds = .data$end_seconds + (1200 * (.data$period-1)),
            duration = lubridate::ms(.data$duration),
            duration_seconds = lubridate::period_to_seconds(.data$duration)
          ) %>%
          dplyr::filter(.data$duration_seconds > 0)
      }

      if (is.null(shifts_raw) || nrow(shifts_raw) == 0) {
        shifts <- tibble::tibble()
      } else {
        shifts <- .aggregate_shifts(shifts_raw)
      }
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no game shifts data for {game_id} available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(shifts)
}


#' Aggregate per-player-shift records into one row per (team, period, time)
#' change, with comma-joined `players_on` / `ids_on` / `players_off` /
#' `ids_off` columns. Shared by both the JSON and HTML code paths so the
#' shape returned by `nhl_game_shifts()` is identical regardless of source.
#' @keywords internal
#' @noRd
.aggregate_shifts <- function(shifts_raw) {
  shifts_on <- shifts_raw %>%
    dplyr::group_by(
      .data$team_name, .data$period, .data$start_time, .data$start_seconds, .data$start_game_seconds
    ) %>%
    dplyr::summarize(
      num_on = dplyr::n(),
      players_on = paste(.data$player_name, collapse = ", "),
      ids_on = paste(.data$player_id, collapse = ", "),
      .groups = "drop"
    ) %>%
    dplyr::rename(
      "period_time" = "start_time",
      "period_seconds" = "start_seconds",
      "game_seconds" = "start_game_seconds"
    )

  shifts_off <- shifts_raw %>%
    dplyr::group_by(
      .data$team_name, .data$period, .data$end_time, .data$end_seconds, .data$end_game_seconds
    ) %>%
    dplyr::summarize(
      num_off = dplyr::n(),
      players_off = paste(.data$player_name, collapse = ", "),
      ids_off = paste(.data$player_id, collapse = ", "),
      .groups = "drop"
    ) %>%
    dplyr::rename(
      "period_time" = "end_time",
      "period_seconds" = "end_seconds",
      "game_seconds" = "end_game_seconds"
    )

  dplyr::full_join(
    shifts_on, shifts_off,
    by = c("game_seconds", "team_name", "period", "period_time", "period_seconds")
  ) %>%
    dplyr::arrange(.data$game_seconds) %>%
    dplyr::mutate(
      event = "Change",
      event_type = "CHANGE",
      game_seconds_remaining = 3600 - .data$game_seconds
    ) %>%
    dplyr::rename("event_team" = "team_name") %>%
    # removing NA values at start and end of periods
    dplyr::mutate(
      players_on = ifelse(is.na(.data$players_on), "None", .data$players_on),
      players_off = ifelse(is.na(.data$players_off), "None", .data$players_off),
      ids_on = ifelse(is.na(.data$ids_on), 0, .data$ids_on),
      ids_off = ifelse(is.na(.data$ids_off), 0, .data$ids_off)
    ) %>%
    make_fastRhockey_data("NHL Game Shifts Information from NHL.com", Sys.time())
}


#' Title-case a name string from an uppercase NHL TOI report
#'
#' `stringr::str_to_title()` alone produces "Mcdonald" / "O'brien" — fix the
#' usual hockey-name prefixes (Mc, Mac, leading apostrophe) so the
#' HTML-fallback `player_name` column matches the proper-case shape the JSON
#' shiftcharts endpoint returns.
#' @keywords internal
#' @noRd
.smart_titlecase <- function(x) {
  if (length(x) == 0 || is.na(x)) return(x)
  s <- stringr::str_to_title(x)
  fix_prefix <- function(s, pattern) {
    stringr::str_replace_all(s, pattern, function(m) {
      paste0(stringr::str_sub(m, 1, -2), toupper(stringr::str_sub(m, -1, -1)))
    })
  }
  # Capitalize the letter immediately following Mc / Mac / a single quote.
  # Order matters: do Mac before Mc so "Macdonald" maps to "MacDonald" not
  # "McAcdonald".
  s <- fix_prefix(s, "\\bMac[a-z]")
  s <- fix_prefix(s, "\\bMc[a-z]")
  s <- fix_prefix(s, "'[a-z]")
  s
}


#' Fallback to the legacy HTML TOI reports
#'
#' For games where `api.nhle.com/stats/rest/en/shiftcharts` returns an empty
#' payload, the per-shift records still exist in the two legacy HTML reports
#' that the NHL game-center UI links to:
#'
#'   `nhl.com/scores/htmlreports/{season}/TH{gameno}.HTM`   (home team)
#'   `nhl.com/scores/htmlreports/{season}/TV{gameno}.HTM`   (visitor team)
#'
#' Each report has one `<td class="teamHeading">` (the full team name),
#' followed by per-player blocks. Each block starts with a
#' `<td class="playerHeading">` containing the player's sweater number plus
#' `LAST, FIRST`, then a 6-column table of shifts:
#' `Shift # | Per | StartElapsed/Remaining | EndElapsed/Remaining | Duration | Event`.
#'
#' We walk both reports' player-headings and shift rows in document order,
#' parse `LAST, FIRST` into a proper-case `Firstname Lastname` (matching the
#' JSON shiftcharts output), normalize the team name to title case
#' (`"BUFFALO SABRES"` -> `"Buffalo Sabres"`), and map sweater numbers back
#' to NHL `player_id`s via `nhl_game_boxscore()`. The result is a tibble in
#' the same shape as the JSON `shifts_raw` so the aggregation pipeline
#' downstream is identical regardless of source.
#' @keywords internal
#' @noRd
.parse_toi_html <- function(game_id) {
  gid <- as.character(game_id)
  if (!grepl("^[0-9]{10}$", gid)) return(NULL)
  season_year <- substr(gid, 1, 4)
  season_str <- paste0(season_year, as.integer(season_year) + 1L)
  gameno <- substr(gid, 5, 10)
  base_path <- paste0("https://www.nhl.com/scores/htmlreports/", season_str)

  parse_side <- function(side_letter) {
    url <- paste0(base_path, "/T", side_letter, gameno, ".HTM")
    html <- tryCatch(rvest::read_html(url), error = function(e) NULL)
    if (is.null(html)) return(NULL)

    team_heads <- html %>%
      rvest::html_nodes("td.teamHeading") %>%
      rvest::html_text(trim = TRUE)
    team_name <- if (length(team_heads) >= 1 && nzchar(team_heads[[1]])) {
      .smart_titlecase(team_heads[[1]])
    } else {
      return(NULL)
    }

    nodes <- xml2::xml_find_all(
      html,
      ".//td[contains(@class, 'playerHeading')] | .//tr[contains(@class, 'oddColor') or contains(@class, 'evenColor')]"
    )
    if (length(nodes) == 0) return(NULL)

    rows <- vector("list", length(nodes))
    cur_sweater <- NA_integer_
    cur_last_first <- NA_character_
    for (i in seq_along(nodes)) {
      n <- nodes[[i]]
      if (xml2::xml_name(n) == "td") {
        txt <- stringr::str_trim(xml2::xml_text(n))
        m <- regmatches(txt, regexec("^([0-9]+)\\s+(.*)$", txt))[[1]]
        if (length(m) == 3L) {
          cur_sweater <- suppressWarnings(as.integer(m[[2]]))
          cur_last_first <- m[[3]]
        }
        next
      }
      cells <- stringr::str_trim(xml2::xml_text(xml2::xml_find_all(n, "./td")))
      if (length(cells) != 6L) next
      if (!grepl("^[0-9]+$", cells[[1]])) next
      if (!grepl("^[0-9]+$", cells[[2]])) next
      if (!grepl(":", cells[[3]])) next
      if (is.na(cur_sweater)) next
      rows[[i]] <- tibble::tibble(
        team_name      = team_name,
        side           = side_letter,
        sweater_number = cur_sweater,
        last_first     = cur_last_first,
        period         = as.integer(cells[[2]]),
        start_time     = sub("^([0-9]+:[0-9]+).*", "\\1", cells[[3]]),
        end_time       = sub("^([0-9]+:[0-9]+).*", "\\1", cells[[4]]),
        duration       = cells[[5]]
      )
    }
    rows <- purrr::compact(rows)
    if (length(rows) == 0L) return(NULL)
    dplyr::bind_rows(rows)
  }

  raw <- dplyr::bind_rows(parse_side("H"), parse_side("V"))
  if (is.null(raw) || nrow(raw) == 0) return(NULL)

  # Convert each "LAST, FIRST" heading (uppercase per the TOI report) into a
  # proper-case "First Last" matching the JSON shiftcharts output. Done once
  # over the unique set so repeated shifts for the same player don't re-run
  # the regex pipeline.
  unique_lf <- unique(raw$last_first)
  name_lookup <- tibble::tibble(
    last_first = unique_lf,
    player_name_html = vapply(unique_lf, function(lf) {
      if (is.na(lf) || !nzchar(lf)) return(NA_character_)
      parts <- stringr::str_split(lf, ",\\s*", n = 2)[[1]]
      if (length(parts) == 2L) {
        last <- .smart_titlecase(stringr::str_trim(parts[[1]]))
        first <- .smart_titlecase(stringr::str_trim(parts[[2]]))
        paste(first, last)
      } else {
        .smart_titlecase(lf)
      }
    }, character(1))
  )
  raw <- raw %>% dplyr::left_join(name_lookup, by = "last_first")

  # Resolve sweater -> player_id via the boxscore. boxscore$skater_stats and
  # $goalie_stats both expose home_away + team_id + team_abbrev + player_id
  # + sweater_number, so a join on (home_away, sweater_number) is unambiguous.
  # The boxscore's player_name column comes through in api-web initial form
  # ("B. Byram"); we prefer the HTML-derived proper-case full name above.
  box <- tryCatch(nhl_game_boxscore(game_id = game_id), error = function(e) NULL)
  if (is.null(box) || is.null(box$skater_stats)) return(NULL)

  lookup <- dplyr::bind_rows(
    if (!is.null(box$skater_stats)) box$skater_stats else NULL,
    if (!is.null(box$goalie_stats)) box$goalie_stats else NULL
  )
  needed_lookup_cols <- c("home_away", "sweater_number", "player_id", "team_id", "team_abbrev")
  if (!all(needed_lookup_cols %in% names(lookup))) return(NULL)
  lookup <- lookup %>%
    dplyr::select(dplyr::all_of(needed_lookup_cols))

  raw <- raw %>%
    dplyr::mutate(home_away = ifelse(.data$side == "H", "home", "away")) %>%
    dplyr::left_join(lookup, by = c("home_away", "sweater_number")) %>%
    dplyr::filter(!is.na(.data$player_id)) %>%
    dplyr::mutate(player_name = dplyr::coalesce(.data$player_name_html, NA_character_))
  if (nrow(raw) == 0) return(NULL)

  raw <- raw %>%
    dplyr::mutate(
      game_id            = as.integer(game_id),
      start_time_ms      = lubridate::ms(.data$start_time),
      start_seconds      = lubridate::period_to_seconds(.data$start_time_ms),
      start_game_seconds = .data$start_seconds + (1200 * (.data$period - 1L)),
      end_time_ms        = lubridate::ms(.data$end_time),
      end_seconds        = lubridate::period_to_seconds(.data$end_time_ms),
      end_game_seconds   = .data$end_seconds + (1200 * (.data$period - 1L)),
      duration_period    = lubridate::ms(.data$duration),
      duration_seconds   = lubridate::period_to_seconds(.data$duration_period),
      duration           = .data$duration_period
    ) %>%
    dplyr::filter(.data$duration_seconds > 0) %>%
    dplyr::select(
      "game_id", "player_id", "player_name", "team_abbrev", "team_id",
      "team_name", "period", "start_time", "end_time", "duration",
      "start_time_ms", "start_seconds", "start_game_seconds",
      "end_time_ms", "end_seconds", "end_game_seconds", "duration_seconds"
    )
  raw
}
