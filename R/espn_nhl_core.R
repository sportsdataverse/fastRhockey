# espn_nhl_core.R
# ESPN NHL: scoreboard, standings, team_roster, calendar, schedule
# Internal league-generic helpers + public NHL shims.

# ===========================================================================
# 1. Scoreboard
# ===========================================================================

#' Internal: ESPN hockey scoreboard (league-generic)
#'
#' @param league ESPN league slug. Defaults to `"nhl"`.
#' @param dates Optional date string `"YYYYMMDD"` or date-range
#'   `"YYYYMMDD-YYYYMMDD"`.
#' @param ... Passed through to `.retry_request()`.
#' @return A `fastRhockey_data` tibble, one row per game.
#' @noRd
.espn_hockey_scoreboard <- function(league = "nhl", dates = NULL, ...) {
  .args <- .capture_args()

  scoreboard <- data.frame()

  tryCatch(
    expr = {
      query <- list()
      if (!is.null(dates)) query[["dates"]] <- dates

      raw <- .espn_hockey_request("site_v2", "scoreboard",
                                  query  = query,
                                  league = league, ...)

      events <- raw[["events"]]
      if (is.null(events) || (is.data.frame(events) && nrow(events) == 0) ||
          (is.list(events) && length(events) == 0)) {
        return(scoreboard)
      }

      # events is a data.frame when simplifyVector = TRUE
      if (!is.data.frame(events)) {
        stop("Unexpected events format", call. = FALSE)
      }

      # Helper: extract a character field from a data.frame safely
      .cfield_sb <- function(df, col) {
        if (is.data.frame(df) && nrow(df) > 0 && col %in% colnames(df))
          as.character(df[[col]])
        else NA_character_
      }

      rows <- lapply(seq_len(nrow(events)), function(i) {
        ev <- events[i, , drop = FALSE]

        # season/status are nested data.frame columns in the events data.frame
        season_df      <- ev$season   # cols: year, type, slug
        status_df      <- ev$status   # cols: clock, displayClock, period, type(df)
        status_type_df <- if (!is.null(status_df) && is.data.frame(status_df) &&
                              !is.null(status_df$type) && is.data.frame(status_df$type))
          status_df$type else data.frame()  # cols: id, name, state, completed, ...

        comps <- if (!is.null(ev$competitions) && is.list(ev$competitions))
          ev$competitions[[1]] else data.frame()

        venue_df <- if (!is.null(comps$venue) && is.data.frame(comps$venue))
          comps$venue else data.frame()
        venue_addr_df <- if (!is.null(venue_df$address) && is.data.frame(venue_df$address))
          venue_df$address else data.frame()

        # competitors is a nested data.frame column; team is a nested df inside it
        competitors_df <- if (!is.null(comps$competitors) && is.list(comps$competitors))
          comps$competitors[[1]] else data.frame()

        home_idx <- NA_integer_
        away_idx <- NA_integer_
        if (is.data.frame(competitors_df) && nrow(competitors_df) > 0 &&
            "homeAway" %in% colnames(competitors_df)) {
          h_idx <- which(competitors_df$homeAway == "home")
          a_idx <- which(competitors_df$homeAway == "away")
          if (length(h_idx) > 0) home_idx <- h_idx[1]
          if (length(a_idx) > 0) away_idx <- a_idx[1]
        }

        # team is a nested data.frame column inside competitors_df
        team_df <- if (is.data.frame(competitors_df) &&
                       !is.null(competitors_df$team) &&
                       is.data.frame(competitors_df$team))
          competitors_df$team else data.frame()

        home_team_df <- if (!is.na(home_idx) && nrow(team_df) >= home_idx)
          team_df[home_idx, , drop = FALSE] else data.frame()
        away_team_df <- if (!is.na(away_idx) && nrow(team_df) >= away_idx)
          team_df[away_idx, , drop = FALSE] else data.frame()

        # logo column on team (scoreboard uses single "logo" string, not logos[])
        home_logo_href <- .cfield_sb(home_team_df, "logo")
        away_logo_href <- .cfield_sb(away_team_df, "logo")

        # broadcast
        broadcast_str <- tryCatch({
          bc_raw <- comps$broadcast
          if (!is.null(bc_raw) && nzchar(as.character(bc_raw %||% "")))
            as.character(bc_raw)
          else ""
        }, error = function(e) "")

        # note
        note_str <- tryCatch({
          nl <- comps$notes
          if (is.list(nl) && length(nl) > 0) {
            n <- nl[[1]]
            if (is.data.frame(n) && "headline" %in% colnames(n)) n$headline[1]
            else ""
          } else ""
        }, error = function(e) "")

        # score / winner: scalar columns on competitors_df
        home_score_val <- if (!is.na(home_idx) && is.data.frame(competitors_df) &&
                              "score" %in% colnames(competitors_df))
          as.character(competitors_df$score[[home_idx]]) else NA_character_
        away_score_val <- if (!is.na(away_idx) && is.data.frame(competitors_df) &&
                              "score" %in% colnames(competitors_df))
          as.character(competitors_df$score[[away_idx]]) else NA_character_
        home_winner_val <- if (!is.na(home_idx) && is.data.frame(competitors_df) &&
                               "winner" %in% colnames(competitors_df))
          as.logical(competitors_df$winner[[home_idx]]) else NA
        away_winner_val <- if (!is.na(away_idx) && is.data.frame(competitors_df) &&
                               "winner" %in% colnames(competitors_df))
          as.logical(competitors_df$winner[[away_idx]]) else NA

        # rank: curatedRank is a nested df column on competitors_df
        home_rank_val <- tryCatch({
          if (!is.na(home_idx) && is.data.frame(competitors_df) &&
              "curatedRank" %in% colnames(competitors_df)) {
            cr_col <- competitors_df$curatedRank
            if (is.data.frame(cr_col) && "current" %in% colnames(cr_col))
              as.integer(cr_col$current[[home_idx]])
            else NA_integer_
          } else NA_integer_
        }, error = function(e) NA_integer_)
        away_rank_val <- tryCatch({
          if (!is.na(away_idx) && is.data.frame(competitors_df) &&
              "curatedRank" %in% colnames(competitors_df)) {
            cr_col <- competitors_df$curatedRank
            if (is.data.frame(cr_col) && "current" %in% colnames(cr_col))
              as.integer(cr_col$current[[away_idx]])
            else NA_integer_
          } else NA_integer_
        }, error = function(e) NA_integer_)

        list(
          game_id                  = ev$id %||% NA_character_,
          uid                      = ev$uid %||% NA_character_,
          date                     = ev$date %||% NA_character_,
          name                     = ev$name %||% NA_character_,
          short_name               = ev$shortName %||% NA_character_,
          season_year              = if (is.data.frame(season_df) && "year" %in% colnames(season_df)) as.integer(season_df$year) else NA_integer_,
          season_type              = if (is.data.frame(season_df) && "type" %in% colnames(season_df)) as.integer(season_df$type) else NA_integer_,
          season_slug              = if (is.data.frame(season_df) && "slug" %in% colnames(season_df)) as.character(season_df$slug) else NA_character_,
          status_type_id           = .cfield_sb(status_type_df, "id"),
          status_type_name         = .cfield_sb(status_type_df, "name"),
          status_type_state        = .cfield_sb(status_type_df, "state"),
          status_type_completed    = if (nrow(status_type_df) > 0 && "completed" %in% colnames(status_type_df)) as.logical(status_type_df$completed[[1]]) else NA,
          status_type_description  = .cfield_sb(status_type_df, "description"),
          status_type_detail       = .cfield_sb(status_type_df, "detail"),
          status_type_short_detail = .cfield_sb(status_type_df, "shortDetail"),
          status_clock             = if (is.data.frame(status_df) && "clock" %in% colnames(status_df)) as.numeric(status_df$clock) else NA_real_,
          status_display_clock     = if (is.data.frame(status_df) && "displayClock" %in% colnames(status_df)) as.character(status_df$displayClock) else NA_character_,
          status_period            = if (is.data.frame(status_df) && "period" %in% colnames(status_df)) as.integer(status_df$period) else NA_integer_,
          neutral_site             = if (!is.null(comps$neutralSite)) as.logical(comps$neutralSite) else NA,
          conference_competition   = comps$conferenceCompetition %||% NA_character_,
          attendance               = if (!is.null(comps$attendance)) as.integer(comps$attendance) else NA_integer_,
          venue_id                 = .cfield_sb(venue_df, "id"),
          venue_full_name          = .cfield_sb(venue_df, "fullName"),
          venue_city               = .cfield_sb(venue_addr_df, "city"),
          venue_state              = .cfield_sb(venue_addr_df, "state"),
          venue_indoor             = if (nrow(venue_df) > 0 && "indoor" %in% colnames(venue_df)) as.logical(venue_df$indoor[[1]]) else NA,
          broadcast                = broadcast_str,
          note                     = note_str,
          home_id                  = .cfield_sb(home_team_df, "id"),
          home_name                = .cfield_sb(home_team_df, "name"),
          home_abbreviation        = .cfield_sb(home_team_df, "abbreviation"),
          home_display_name        = .cfield_sb(home_team_df, "displayName"),
          home_location            = .cfield_sb(home_team_df, "location"),
          home_color               = .cfield_sb(home_team_df, "color"),
          home_alternate_color     = .cfield_sb(home_team_df, "alternateColor"),
          home_logo                = home_logo_href,
          home_score               = home_score_val,
          home_winner              = home_winner_val,
          home_rank                = home_rank_val,
          away_id                  = .cfield_sb(away_team_df, "id"),
          away_name                = .cfield_sb(away_team_df, "name"),
          away_abbreviation        = .cfield_sb(away_team_df, "abbreviation"),
          away_display_name        = .cfield_sb(away_team_df, "displayName"),
          away_location            = .cfield_sb(away_team_df, "location"),
          away_color               = .cfield_sb(away_team_df, "color"),
          away_alternate_color     = .cfield_sb(away_team_df, "alternateColor"),
          away_logo                = away_logo_href,
          away_score               = away_score_val,
          away_winner              = away_winner_val,
          away_rank                = away_rank_val
        )
      })

      scoreboard <- dplyr::bind_rows(lapply(rows, function(r) {
        as.data.frame(r, stringsAsFactors = FALSE)
      })) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Scoreboard data from ESPN.com"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} scoreboard data available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} scoreboard",
        args = .args)
    },
    finally = {}
  )
  return(scoreboard)
}

# ---------------------------------------------------------------------------
# Public shim
# ---------------------------------------------------------------------------

#' @name espn_nhl_scoreboard
NULL
#' @title **Get ESPN NHL Scoreboard**
#' @rdname espn_nhl_scoreboard
#' @author Saiem Gilani
#' @param dates Optional date or date-range string in `"YYYYMMDD"` or
#'   `"YYYYMMDD-YYYYMMDD"` format. When `NULL` (default) the current day
#'   is used.
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per game:
#'
#'    |col_name                 |types     |description                                      |
#'    |:------------------------|:---------|:------------------------------------------------|
#'    |game_id                  |character |ESPN event identifier.                           |
#'    |uid                      |character |ESPN event uid.                                  |
#'    |date                     |character |Game date-time (ISO 8601).                       |
#'    |name                     |character |Full game name.                                  |
#'    |short_name               |character |Short game name.                                 |
#'    |season_year              |integer   |Season end year.                                 |
#'    |season_type              |integer   |Season type code (1=pre, 2=regular, 3=post).     |
#'    |season_slug              |character |Season type slug.                                |
#'    |status_type_id           |character |Status type identifier.                          |
#'    |status_type_name         |character |Status type name.                                |
#'    |status_type_state        |character |Status state (pre/in/post).                      |
#'    |status_type_completed    |logical   |Whether the game is complete.                    |
#'    |status_type_description  |character |Status description.                              |
#'    |status_type_detail       |character |Status detail text.                              |
#'    |status_type_short_detail |character |Short status detail.                             |
#'    |status_clock             |numeric   |Game clock in seconds.                           |
#'    |status_display_clock     |character |Display clock string.                            |
#'    |status_period            |integer   |Current period.                                  |
#'    |neutral_site             |logical   |Whether the game is at a neutral site.           |
#'    |conference_competition   |character |Whether it is a conference competition.          |
#'    |attendance               |integer   |Game attendance.                                 |
#'    |venue_id                 |character |ESPN venue identifier.                           |
#'    |venue_full_name          |character |Venue full name.                                 |
#'    |venue_city               |character |Venue city.                                      |
#'    |venue_state              |character |Venue state.                                     |
#'    |venue_indoor             |logical   |Whether the venue is indoors.                    |
#'    |broadcast                |character |Broadcast network(s).                            |
#'    |note                     |character |Game note or headline.                           |
#'    |home_id                  |character |Home team ESPN identifier.                       |
#'    |home_name                |character |Home team name.                                  |
#'    |home_abbreviation        |character |Home team abbreviation.                          |
#'    |home_display_name        |character |Home team display name.                          |
#'    |home_location            |character |Home team city/location.                         |
#'    |home_color               |character |Home team primary color hex.                     |
#'    |home_alternate_color     |character |Home team alternate color hex.                   |
#'    |home_logo                |character |Home team logo URL.                              |
#'    |home_score               |character |Home team score.                                 |
#'    |home_winner              |logical   |Whether the home team won.                       |
#'    |home_rank                |integer   |Home team rank (if ranked).                      |
#'    |away_id                  |character |Away team ESPN identifier.                       |
#'    |away_name                |character |Away team name.                                  |
#'    |away_abbreviation        |character |Away team abbreviation.                          |
#'    |away_display_name        |character |Away team display name.                          |
#'    |away_location            |character |Away team city/location.                         |
#'    |away_color               |character |Away team primary color hex.                     |
#'    |away_alternate_color     |character |Away team alternate color hex.                   |
#'    |away_logo                |character |Away team logo URL.                              |
#'    |away_score               |character |Away team score.                                 |
#'    |away_winner              |logical   |Whether the away team won.                       |
#'    |away_rank                |integer   |Away team rank (if ranked).                      |
#'
#' @importFrom dplyr bind_rows mutate
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_scoreboard(dates = "20250110"))
#' }
espn_nhl_scoreboard <- function(dates = NULL, ...) {
  .espn_hockey_scoreboard(league = "nhl", dates = dates, ...)
}


# ===========================================================================
# 2. Standings
# ===========================================================================

#' Internal: ESPN hockey standings (league-generic)
#'
#' @param league ESPN league slug.
#' @param season Season end-year (e.g. `2025`).
#' @param ... Passed through.
#' @return A `fastRhockey_data` tibble, one row per team.
#' @noRd
.espn_hockey_standings <- function(league = "nhl",
                                   season = most_recent_nhl_season(),
                                   ...) {
  .args <- .capture_args()

  standings <- data.frame()

  tryCatch(
    expr = {
      query <- list()
      if (!is.null(season)) query[["season"]] <- season

      # Must use site_v2_alt (apis/v2/…) — plain site_v2/standings is a stub.
      raw <- .espn_hockey_request("site_v2_alt", "standings",
                                  query  = query,
                                  league = league,
                                  simplifyVector = FALSE, ...)

      children <- raw[["children"]] %||% list()
      if (length(children) == 0) return(standings)

      .extract_entries <- function(child_list, parent_name = "", parent_abbr = "") {
        rows <- list()
        for (child in child_list) {
          g_name <- child$name %||% parent_name
          g_abbr <- child$abbreviation %||% parent_abbr
          stl    <- child$standings %||% list()
          entries <- stl$entries %||% list()
          if (length(entries) > 0) {
            for (entry in entries) {
              team     <- entry$team %||% list()
              stats_list <- entry$stats %||% list()
              row <- list(
                group_name         = g_name,
                group_abbreviation = g_abbr,
                team_id            = team$id %||% NA_character_,
                team_name          = team$name %||% NA_character_,
                team_abbreviation  = team$abbreviation %||% NA_character_,
                team_display_name  = team$displayName %||% NA_character_,
                team_location      = team$location %||% NA_character_,
                team_logo          = if (!is.null(team$logos) && length(team$logos) > 0)
                  team$logos[[1]]$href %||% NA_character_ else NA_character_
              )
              for (stat in stats_list) {
                col <- janitor::make_clean_names(stat$name %||% stat$abbreviation %||% "stat")
                row[[col]] <- stat$value %||% NA_real_
              }
              rows <- c(rows, list(row))
            }
          }
          sub_children <- child$children %||% list()
          if (length(sub_children) > 0) {
            rows <- c(rows, .extract_entries(sub_children, g_name, g_abbr))
          }
        }
        rows
      }

      rows <- .extract_entries(children)
      if (length(rows) == 0) return(standings)

      standings <- dplyr::bind_rows(lapply(rows, function(r) {
        as.data.frame(r, stringsAsFactors = FALSE)
      })) %>%
        dplyr::mutate(season = as.integer(season)) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Standings data from ESPN.com"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} standings data available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} standings",
        args = .args)
    },
    finally = {}
  )
  return(standings)
}

# ---------------------------------------------------------------------------
# Public shim
# ---------------------------------------------------------------------------

#' @name espn_nhl_standings
NULL
#' @title **Get ESPN NHL Standings**
#' @rdname espn_nhl_standings
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2025`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per team:
#'
#'    |col_name            |types     |description                                |
#'    |:-------------------|:---------|:------------------------------------------|
#'    |group_name          |character |Group name (conference / division).        |
#'    |group_abbreviation  |character |Group abbreviation.                        |
#'    |team_id             |character |ESPN team identifier.                      |
#'    |team_name           |character |Team name.                                 |
#'    |team_abbreviation   |character |Team abbreviation.                         |
#'    |team_display_name   |character |Team display name.                         |
#'    |team_location       |character |Team location (city).                      |
#'    |team_logo           |character |Team logo URL.                             |
#'    |season              |integer   |Season end-year (echoed from arg).         |
#'    |ot_losses           |numeric   |Overtime losses.                           |
#'    |games_played        |numeric   |Games played.                              |
#'    |losses              |numeric   |Regulation losses.                         |
#'    |points              |numeric   |Points.                                    |
#'    |wins                |numeric   |Wins.                                      |
#'    |points_for          |numeric   |Goals/points scored.                       |
#'    |points_against      |numeric   |Goals/points allowed.                      |
#'    |playoff_seed        |numeric   |Current playoff seed.                      |
#'    |streak              |numeric   |Current streak value.                      |
#'    |overall             |numeric   |Overall record stat.                       |
#'
#' @importFrom dplyr bind_rows mutate
#' @importFrom janitor clean_names make_clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_standings(season = 2025))
#' }
espn_nhl_standings <- function(season = most_recent_nhl_season(), ...) {
  .espn_hockey_standings(league = "nhl", season = season, ...)
}


# ===========================================================================
# 3. Team Roster
# ===========================================================================

#' Internal: ESPN hockey team roster (league-generic)
#'
#' @param league ESPN league slug.
#' @param team_id ESPN team identifier.
#' @param ... Passed through.
#' @return A `fastRhockey_data` tibble, one row per player.
#' @noRd
.espn_hockey_team_roster <- function(league = "nhl", team_id, ...) {
  .args <- .capture_args()

  roster <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("teams/", team_id, "/roster")
      raw  <- .espn_hockey_request("site_v2", path,
                                   league = league,
                                   simplifyVector = FALSE, ...)

      athletes <- raw[["athletes"]] %||% list()
      if (length(athletes) == 0) return(roster)

      # NHL uses position-grouped shape: athletes[] = list of {position, items[]}
      first <- athletes[[1]]
      is_grouped <- is.list(first) && "position" %in% names(first) &&
        is.list(first[["items"]])

      rows <- list()
      if (is_grouped) {
        for (grp in athletes) {
          grp_name <- grp$position %||% NA_character_
          items    <- grp$items %||% list()
          for (player in items) {
            pos    <- player$position %||% list()
            pos_parent <- pos$parent %||% list()
            hs     <- player$headshot %||% list()
            hand   <- player$hand %||% list()
            stat   <- player$status %||% list()
            exp    <- player$experience %||% list()
            bp     <- player$birthPlace %||% list()
            bc     <- player$birthCountry %||% list()
            alt_ids <- player$alternateIds %||% list()
            college <- player$college %||% list()
            college_logos <- college$logos %||% list()

            row <- list(
              position_group              = grp_name,
              id                          = player$id %||% NA_character_,
              uid                         = player$uid %||% NA_character_,
              guid                        = player$guid %||% NA_character_,
              alternate_id                = player$alternateId %||% NA_character_,
              alternate_ids_sdr           = alt_ids$sdr %||% NA_character_,
              first_name                  = player$firstName %||% NA_character_,
              last_name                   = player$lastName %||% NA_character_,
              full_name                   = player$fullName %||% NA_character_,
              display_name                = player$displayName %||% NA_character_,
              short_name                  = player$shortName %||% NA_character_,
              weight                      = if (!is.null(player$weight)) as.numeric(player$weight) else NA_real_,
              display_weight              = player$displayWeight %||% NA_character_,
              height                      = if (!is.null(player$height)) as.numeric(player$height) else NA_real_,
              display_height              = player$displayHeight %||% NA_character_,
              age                         = if (!is.null(player$age)) as.integer(player$age) else NA_integer_,
              date_of_birth               = player$dateOfBirth %||% NA_character_,
              slug                        = player$slug %||% NA_character_,
              jersey                      = player$jersey %||% NA_character_,
              headshot_href               = hs$href %||% NA_character_,
              headshot_alt                = hs$alt %||% NA_character_,
              hand_type                   = hand$type %||% NA_character_,
              hand_abbreviation           = hand$abbreviation %||% NA_character_,
              hand_display_value          = hand$displayValue %||% NA_character_,
              position_id                 = pos$id %||% NA_character_,
              position_name               = pos$name %||% NA_character_,
              position_display_name       = pos$displayName %||% NA_character_,
              position_abbreviation       = pos$abbreviation %||% NA_character_,
              position_leaf               = if (!is.null(pos$leaf)) as.logical(pos$leaf) else NA,
              position_parent_id          = pos_parent$id %||% NA_character_,
              position_parent_name        = pos_parent$name %||% NA_character_,
              position_parent_display_name = pos_parent$displayName %||% NA_character_,
              position_parent_abbreviation = pos_parent$abbreviation %||% NA_character_,
              position_parent_leaf        = if (!is.null(pos_parent$leaf)) as.logical(pos_parent$leaf) else NA,
              experience_years            = if (!is.null(exp$years)) as.integer(exp$years) else NA_integer_,
              debut_year                  = if (!is.null(player$debutYear)) as.numeric(player$debutYear) else NA_real_,
              status_id                   = stat$id %||% NA_character_,
              status_name                 = stat$name %||% NA_character_,
              status_type                 = stat$type %||% NA_character_,
              status_abbreviation         = stat$abbreviation %||% NA_character_,
              birth_place_city            = bp$city %||% NA_character_,
              birth_place_state           = bp$state %||% NA_character_,
              birth_place_country         = bp$country %||% NA_character_,
              birth_place_display_text    = bp$displayText %||% NA_character_,
              birth_country_abbreviation  = bc$abbreviation %||% NA_character_,
              college_id                  = college$id %||% NA_character_,
              college_guid                = college$guid %||% NA_character_,
              college_mascot              = college$mascot %||% NA_character_,
              college_name                = college$name %||% NA_character_,
              college_short_name          = college$shortName %||% NA_character_,
              college_abbrev              = college$abbrev %||% NA_character_,
              college_logos               = if (length(college_logos) > 0)
                paste(sapply(college_logos, function(l) l$href %||% ""), collapse = "|")
              else NA_character_,
              team_id                     = as.character(team_id)
            )
            rows <- c(rows, list(row))
          }
        }
      } else {
        # Flat shape (other sports)
        for (player in athletes) {
          rows <- c(rows, list(c(player, list(team_id = as.character(team_id)))))
        }
      }

      if (length(rows) == 0) return(roster)

      roster <- dplyr::bind_rows(lapply(rows, function(r) {
        as.data.frame(r, stringsAsFactors = FALSE)
      })) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Team Roster data from ESPN.com"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} team roster data available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} team roster",
        args = .args)
    },
    finally = {}
  )
  return(roster)
}

# ---------------------------------------------------------------------------
# Public shim
# ---------------------------------------------------------------------------

#' @name espn_nhl_team_roster
NULL
#' @title **Get ESPN NHL Team Roster**
#' @rdname espn_nhl_team_roster
#' @author Saiem Gilani
#' @param team_id ESPN team identifier (character or numeric, e.g. `"4"` for
#'   Pittsburgh Penguins).
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per player:
#'
#'    |col_name                      |types     |description                         |
#'    |:-----------------------------|:---------|:-----------------------------------|
#'    |position_group                |character |Position group name (e.g. Centers). |
#'    |id                            |character |ESPN player identifier.             |
#'    |uid                           |character |ESPN player uid.                    |
#'    |guid                          |character |Player global unique identifier.    |
#'    |alternate_id                  |character |Alternate player identifier.        |
#'    |alternate_ids_sdr             |character |Alternate ids sdr.                  |
#'    |first_name                    |character |First name.                         |
#'    |last_name                     |character |Last name.                          |
#'    |full_name                     |character |Full name.                          |
#'    |display_name                  |character |Display name.                       |
#'    |short_name                    |character |Short name.                         |
#'    |weight                        |numeric   |Weight (lbs).                       |
#'    |display_weight                |character |Display weight string.              |
#'    |height                        |numeric   |Height (inches).                    |
#'    |display_height                |character |Display height string.              |
#'    |age                           |integer   |Player age.                         |
#'    |date_of_birth                 |character |Date of birth.                      |
#'    |slug                          |character |URL slug.                           |
#'    |jersey                        |character |Jersey number.                      |
#'    |headshot_href                 |character |Headshot image URL.                 |
#'    |headshot_alt                  |character |Headshot alt text.                  |
#'    |hand_type                     |character |Shooting/catching hand type.        |
#'    |hand_abbreviation             |character |Hand abbreviation.                  |
#'    |hand_display_value            |character |Hand display value.                 |
#'    |position_id                   |character |Position identifier.                |
#'    |position_name                 |character |Position name.                      |
#'    |position_display_name         |character |Position display name.              |
#'    |position_abbreviation         |character |Position abbreviation.              |
#'    |position_leaf                 |logical   |Whether position is a leaf node.    |
#'    |position_parent_id            |character |Parent position identifier.         |
#'    |position_parent_name          |character |Parent position name.               |
#'    |position_parent_display_name  |character |Parent position display name.       |
#'    |position_parent_abbreviation  |character |Parent position abbreviation.       |
#'    |position_parent_leaf          |logical   |Whether parent position is leaf.    |
#'    |experience_years              |integer   |Experience years.                   |
#'    |debut_year                    |numeric   |Debut year.                         |
#'    |status_id                     |character |Status identifier.                  |
#'    |status_name                   |character |Status name.                        |
#'    |status_type                   |character |Status type.                        |
#'    |status_abbreviation           |character |Status abbreviation.                |
#'    |birth_place_city              |character |Birth place city.                   |
#'    |birth_place_state             |character |Birth place state.                  |
#'    |birth_place_country           |character |Birth place country.                |
#'    |birth_place_display_text      |character |Birth place display text.           |
#'    |birth_country_abbreviation    |character |Birth country abbreviation.         |
#'    |college_id                    |character |College identifier.                 |
#'    |college_guid                  |character |College guid.                       |
#'    |college_mascot                |character |College mascot.                     |
#'    |college_name                  |character |College name.                       |
#'    |college_short_name            |character |College short name.                 |
#'    |college_abbrev                |character |College abbreviation.               |
#'    |college_logos                 |character |College logo URLs (pipe-delimited). |
#'    |team_id                       |character |ESPN team identifier (from arg).    |
#'
#' @importFrom dplyr bind_rows mutate
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_team_roster(team_id = "4"))
#' }
espn_nhl_team_roster <- function(team_id, ...) {
  .espn_hockey_team_roster(league = "nhl", team_id = team_id, ...)
}


# ===========================================================================
# 4. Calendar
# ===========================================================================

#' Internal: ESPN hockey calendar (league-generic)
#'
#' The NHL does not expose a dedicated `/calendar` path; this function
#' extracts the game-date list from the `leagues[].calendar` field of the
#' scoreboard payload.
#'
#' @param league ESPN league slug.
#' @param ... Passed through.
#' @return A `fastRhockey_data` tibble with one row per calendar date.
#' @noRd
.espn_hockey_calendar <- function(league = "nhl", ...) {
  .args <- .capture_args()

  calendar <- data.frame()

  tryCatch(
    expr = {
      raw <- .espn_hockey_request("site_v2", "scoreboard",
                                  league = league, ...)

      leagues <- raw[["leagues"]]
      if (is.null(leagues) || (!is.data.frame(leagues) && !is.list(leagues)) ||
          length(leagues) == 0) {
        return(calendar)
      }

      league_row <- if (is.data.frame(leagues)) leagues[1, , drop = FALSE] else leagues[[1]]

      cal_type  <- if (is.data.frame(league_row)) league_row$calendarType   else league_row[["calendarType"]]
      cal_start <- if (is.data.frame(league_row)) league_row$calendarStartDate else league_row[["calendarStartDate"]]
      cal_end   <- if (is.data.frame(league_row)) league_row$calendarEndDate else league_row[["calendarEndDate"]]
      cal_wl    <- if (is.data.frame(league_row)) league_row$calendarIsWhitelist else league_row[["calendarIsWhitelist"]]

      dates_raw <- if (is.data.frame(league_row)) league_row$calendar[[1]] else league_row[["calendar"]]
      if (is.null(dates_raw) || length(dates_raw) == 0) return(calendar)

      dates_vec <- unlist(dates_raw)

      df_rows <- data.frame(
        date                   = as.character(dates_vec),
        calendar_type          = as.character(cal_type %||% NA_character_),
        calendar_start_date    = as.character(cal_start %||% NA_character_),
        calendar_end_date      = as.character(cal_end %||% NA_character_),
        calendar_is_whitelist  = as.logical(cal_wl %||% NA),
        stringsAsFactors       = FALSE
      )

      calendar <- df_rows %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Calendar data from ESPN.com"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} calendar data available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} calendar",
        args = .args)
    },
    finally = {}
  )
  return(calendar)
}

# ---------------------------------------------------------------------------
# Public shim
# ---------------------------------------------------------------------------

#' @name espn_nhl_calendar
NULL
#' @title **Get ESPN NHL Calendar**
#' @rdname espn_nhl_calendar
#' @author Saiem Gilani
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per game date:
#'
#'    |col_name                |types     |description                                         |
#'    |:-----------------------|:---------|:---------------------------------------------------|
#'    |date                    |character |Game date (ISO 8601 datetime string).               |
#'    |calendar_type           |character |Calendar type (e.g. `"day"`).                       |
#'    |calendar_start_date     |character |Calendar season start date.                         |
#'    |calendar_end_date       |character |Calendar season end date.                           |
#'    |calendar_is_whitelist   |logical   |Whether dates are a whitelist of game days.         |
#'
#' @importFrom dplyr mutate
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_calendar())
#' }
espn_nhl_calendar <- function(...) {
  .espn_hockey_calendar(league = "nhl", ...)
}


# ===========================================================================
# 5. Schedule
# ===========================================================================

#' Internal: ESPN hockey team schedule (league-generic)
#'
#' @param league ESPN league slug.
#' @param team_id ESPN team identifier.
#' @param season Season end-year (e.g. `2025`).
#' @param ... Passed through.
#' @return A `fastRhockey_data` tibble, one row per game.
#' @noRd
.espn_hockey_schedule <- function(league = "nhl",
                                  team_id,
                                  season = most_recent_nhl_season(),
                                  ...) {
  .args <- .capture_args()

  schedule <- data.frame()

  tryCatch(
    expr = {
      path  <- paste0("teams/", team_id, "/schedule")
      query <- list()
      if (!is.null(season)) query[["season"]] <- season

      raw <- .espn_hockey_request("site_v2", path,
                                  query  = query,
                                  league = league,
                                  simplifyVector = FALSE, ...)

      events <- raw[["events"]] %||% list()
      if (length(events) == 0) return(schedule)

      rows <- lapply(events, function(ev) {
        comps <- if (length(ev$competitions) > 0) ev$competitions[[1]] else list()
        competitors <- comps$competitors %||% list()
        home_list <- Filter(function(c) !is.null(c$homeAway) && c$homeAway == "home", competitors)
        away_list <- Filter(function(c) !is.null(c$homeAway) && c$homeAway == "away", competitors)
        home <- if (length(home_list) > 0) home_list[[1]] else list()
        away <- if (length(away_list) > 0) away_list[[1]] else list()
        home_team <- home$team %||% list()
        away_team <- away$team %||% list()
        venue     <- comps$venue %||% list()
        venue_addr <- venue$address %||% list()
        status    <- comps$status %||% list()
        st_type   <- status$type %||% list()

        home_score_raw <- home$score %||% list()
        away_score_raw <- away$score %||% list()
        home_score_val <- if (is.list(home_score_raw)) home_score_raw$value %||% NA_character_ else as.character(home_score_raw %||% NA_character_)
        away_score_val <- if (is.list(away_score_raw)) away_score_raw$value %||% NA_character_ else as.character(away_score_raw %||% NA_character_)

        broadcast_str <- tryCatch({
          bc <- comps$broadcasts %||% list()
          if (length(bc) > 0) {
            nms <- bc[[1]]$names %||% list()
            if (length(nms) > 0) paste(nms, collapse = ", ") else ""
          } else ""
        }, error = function(e) "")

        list(
          game_id            = ev$id %||% NA_character_,
          date               = ev$date %||% NA_character_,
          name               = ev$name %||% NA_character_,
          short_name         = ev$shortName %||% NA_character_,
          season_year        = if (!is.null(ev$season)) as.integer(ev$season$year %||% NA) else NA_integer_,
          season_type        = if (!is.null(ev$seasonType)) as.integer(ev$seasonType$id %||% NA) else NA_integer_,
          time_valid         = if (!is.null(ev$timeValid)) as.logical(ev$timeValid) else NA,
          status_type_id     = st_type$id %||% NA_character_,
          status_type_name   = st_type$name %||% NA_character_,
          status_type_state  = st_type$state %||% NA_character_,
          status_type_completed = if (!is.null(st_type$completed)) as.logical(st_type$completed) else NA,
          neutral_site       = if (!is.null(comps$neutralSite)) as.logical(comps$neutralSite) else NA,
          attendance         = if (!is.null(comps$attendance)) as.integer(comps$attendance) else NA_integer_,
          venue_id           = venue$id %||% NA_character_,
          venue_full_name    = venue$fullName %||% NA_character_,
          venue_city         = venue_addr$city %||% NA_character_,
          venue_state        = venue_addr$state %||% NA_character_,
          broadcast          = broadcast_str,
          home_id            = home_team$id %||% NA_character_,
          home_name          = home_team$displayName %||% NA_character_,
          home_abbreviation  = home_team$abbreviation %||% NA_character_,
          home_location      = home_team$location %||% NA_character_,
          home_logo          = if (length(home_team$logos %||% list()) > 0)
            home_team$logos[[1]]$href %||% NA_character_ else NA_character_,
          home_score         = home_score_val,
          home_winner        = if (!is.null(home$winner)) as.logical(home$winner) else NA,
          away_id            = away_team$id %||% NA_character_,
          away_name          = away_team$displayName %||% NA_character_,
          away_abbreviation  = away_team$abbreviation %||% NA_character_,
          away_location      = away_team$location %||% NA_character_,
          away_logo          = if (length(away_team$logos %||% list()) > 0)
            away_team$logos[[1]]$href %||% NA_character_ else NA_character_,
          away_score         = away_score_val,
          away_winner        = if (!is.null(away$winner)) as.logical(away$winner) else NA,
          team_id            = as.character(team_id),
          season             = as.integer(season)
        )
      })

      schedule <- dplyr::bind_rows(lapply(rows, function(r) {
        as.data.frame(r, stringsAsFactors = FALSE)
      })) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Team Schedule data from ESPN.com"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} schedule data available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} schedule",
        args = .args)
    },
    finally = {}
  )
  return(schedule)
}

# ---------------------------------------------------------------------------
# Public shim
# ---------------------------------------------------------------------------

#' @name espn_nhl_schedule
NULL
#' @title **Get ESPN NHL Team Schedule**
#' @rdname espn_nhl_schedule
#' @author Saiem Gilani
#' @param team_id ESPN team identifier (character or numeric, e.g. `"4"` for
#'   Pittsburgh Penguins).
#' @param season Season end-year (e.g. `2025`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per game:
#'
#'    |col_name              |types     |description                                        |
#'    |:---------------------|:---------|:--------------------------------------------------|
#'    |game_id               |character |ESPN event identifier.                             |
#'    |date                  |character |Game date-time (ISO 8601).                         |
#'    |name                  |character |Full game name.                                    |
#'    |short_name            |character |Short game name.                                   |
#'    |season_year           |integer   |Season end year.                                   |
#'    |season_type           |integer   |Season type code.                                  |
#'    |time_valid            |logical   |Whether the game time is confirmed.                |
#'    |status_type_id        |character |Status type identifier.                            |
#'    |status_type_name      |character |Status type name.                                  |
#'    |status_type_state     |character |Status state (pre/in/post).                        |
#'    |status_type_completed |logical   |Whether the game is complete.                      |
#'    |neutral_site          |logical   |Whether the game is at a neutral site.             |
#'    |attendance            |integer   |Game attendance.                                   |
#'    |venue_id              |character |Venue identifier.                                  |
#'    |venue_full_name       |character |Venue full name.                                   |
#'    |venue_city            |character |Venue city.                                        |
#'    |venue_state           |character |Venue state.                                       |
#'    |broadcast             |character |Broadcast network(s).                              |
#'    |home_id               |character |Home team ESPN identifier.                         |
#'    |home_name             |character |Home team display name.                            |
#'    |home_abbreviation     |character |Home team abbreviation.                            |
#'    |home_location         |character |Home team city.                                    |
#'    |home_logo             |character |Home team logo URL.                                |
#'    |home_score            |character |Home team score.                                   |
#'    |home_winner           |logical   |Whether the home team won.                         |
#'    |away_id               |character |Away team ESPN identifier.                         |
#'    |away_name             |character |Away team display name.                            |
#'    |away_abbreviation     |character |Away team abbreviation.                            |
#'    |away_location         |character |Away team city.                                    |
#'    |away_logo             |character |Away team logo URL.                                |
#'    |away_score            |character |Away team score.                                   |
#'    |away_winner           |logical   |Whether the away team won.                         |
#'    |team_id               |character |ESPN team identifier (echoed from arg).            |
#'    |season                |integer   |Season end-year (echoed from arg).                 |
#'
#' @importFrom dplyr bind_rows mutate
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_schedule(team_id = "4", season = 2025))
#' }
espn_nhl_schedule <- function(team_id,
                               season = most_recent_nhl_season(),
                               ...) {
  .espn_hockey_schedule(league = "nhl", team_id = team_id,
                         season = season, ...)
}
