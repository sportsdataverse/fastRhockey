#' @title **NHL Game Content**
#' @description Returns game summary content for a given game ID.
#' Uses the new NHL API gamecenter landing endpoint (`api-web.nhle.com`).
#'
#' **Note:** The original `statsapi.web.nhl.com` endpoint has been retired.
#' This function now returns game summary data (teams, scoring, three stars)
#' from the gamecenter landing page instead of the old highlights/content data.
#'
#' @param game_id Game unique ID (e.g., 2024020001)
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name           |types     |description                                  |
#'    |:------------------|:---------|:--------------------------------------------|
#'    |game_id            |integer   |Unique game identifier.                      |
#'    |season             |integer   |Season (concluding year, YYYY).              |
#'    |game_type          |integer   |Game type code (2 = regular, 3 = playoffs).  |
#'    |game_date          |character |Game date.                                   |
#'    |venue              |character |Venue name.                                  |
#'    |away_team_abbrev   |character |Away team abbreviation.                      |
#'    |away_team_name     |character |Away team name.                              |
#'    |away_team_score    |integer   |Away team final score.                       |
#'    |away_team_sog      |integer   |Away team shots on goal.                     |
#'    |home_team_abbrev   |character |Home team abbreviation.                      |
#'    |home_team_name     |character |Home team name.                              |
#'    |home_team_score    |integer   |Home team final score.                       |
#'    |home_team_sog      |integer   |Home team shots on goal.                     |
#'    |game_state         |character |Game state (e.g., FINAL, LIVE).              |
#'    |period             |integer   |Last period number played.                   |
#'    |period_type        |character |Last period type (REG, OT, SO).              |
#'    |star_1_player_id   |integer   |First star player identifier.                |
#'    |star_1_name        |character |First star player name.                      |
#'    |star_1_team_abbrev |character |First star player team abbreviation.         |
#'    |star_1_position    |character |First star player position.                  |
#'    |star_2_player_id   |integer   |Second star player identifier.               |
#'    |star_2_name        |character |Second star player name.                     |
#'    |star_2_team_abbrev |character |Second star player team abbreviation.        |
#'    |star_2_position    |character |Second star player position.                 |
#'    |star_3_player_id   |integer   |Third star player identifier.                |
#'    |star_3_name        |character |Third star player name.                      |
#'    |star_3_team_abbrev |character |Third star player team abbreviation.         |
#'    |star_3_position    |character |Third star player position.                  |
#' @keywords NHL Game Content
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr tibble bind_rows
#' @importFrom glue glue
#' @importFrom janitor clean_names
#' @export
#' @examples
#' \donttest{
#'   try(nhl_game_content(game_id = 2024020001))
#' }
nhl_game_content <- function(game_id) {
  url <- glue::glue("https://api-web.nhle.com/v1/gamecenter/{game_id}/landing")

  tryCatch(
    expr = {
      res <- httr::RETRY("GET", url)
      check_status(res)

      resp_text <- httr::content(res, as = "text", encoding = "UTF-8")
      raw <- jsonlite::fromJSON(resp_text, flatten = TRUE)

      # Extract safe helper
      .safe <- function(x, default = NA_character_) {
        if (is.null(x)) default else x
      }
      .safe_default <- function(x, default = NA_character_) {
        if (is.null(x)) return(default)
        if (is.list(x) && !is.null(x[["default"]])) return(x[["default"]])
        return(as.character(x))
      }

      # Build a summary row
      game_df <- dplyr::tibble(
        game_id = .safe(raw$id, NA_integer_),
        season = .safe(raw$season, NA_integer_),
        game_type = .safe(raw$gameType, NA_integer_),
        game_date = .safe(raw$gameDate),
        venue = .safe_default(raw$venue$default),
        away_team_abbrev = .safe(raw$awayTeam$abbrev),
        away_team_name = .safe_default(raw$awayTeam$commonName),
        away_team_score = .safe(raw$awayTeam$score, NA_integer_),
        away_team_sog = .safe(raw$awayTeam$sog, NA_integer_),
        home_team_abbrev = .safe(raw$homeTeam$abbrev),
        home_team_name = .safe_default(raw$homeTeam$commonName),
        home_team_score = .safe(raw$homeTeam$score, NA_integer_),
        home_team_sog = .safe(raw$homeTeam$sog, NA_integer_),
        game_state = .safe(raw$gameState),
        period = .safe(raw$periodDescriptor$number, NA_integer_),
        period_type = .safe(raw$periodDescriptor$periodType)
      )

      # Extract three stars if available
      if (!is.null(raw$summary) && !is.null(raw$summary$threeStars)) {
        stars <- raw$summary$threeStars
        for (i in seq_len(min(nrow(stars), 3))) {
          star_num <- stars$star[i]
          prefix <- paste0("star_", star_num)
          game_df[[paste0(prefix, "_player_id")]] <- .safe(stars$playerId[i], NA_integer_)
          game_df[[paste0(prefix, "_name")]] <- .safe(stars$name$default[i])
          game_df[[paste0(prefix, "_team_abbrev")]] <- .safe(stars$teamAbbrev[i])
          game_df[[paste0(prefix, "_position")]] <- .safe(stars$position[i])
        }
      }

      game_df <- game_df %>%
        janitor::clean_names() %>%
        make_fastRhockey_data("NHL Game Content Information from NHL.com", Sys.time())
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no game content data for {game_id} available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(game_df)
}
