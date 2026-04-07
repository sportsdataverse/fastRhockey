#' @title **NHL Game Boxscore**
#' @description Retrieve boxscore data for a specific NHL game from the
#'   NHL web API (`api-web.nhle.com`).
#'
#' @param game_id *(integer)* NHL game ID, e.g. `2024020001`.
#' @return A named `list` containing:
#'
#'   * **game_info** — one-row tibble with game metadata (id, season,
#'     game type, date, venue, teams, final score, shots on goal).
#'   * **team_box** — two-row tibble (away / home) with team-level totals.
#'   * **skater_stats** — tibble of all skater (forward + defense)
#'     individual stats for both teams.
#'   * **goalie_stats** — tibble of all goalie individual stats for both
#'     teams.
#'
#' @details
#' Uses the endpoint
#' `https://api-web.nhle.com/v1/gamecenter/{game_id}/boxscore`.
#'
#' @examples
#' \dontrun{
#'   box <- nhl_game_boxscore(2024020001)
#'   box$game_info
#'   box$skater_stats
#' }
#'
#' @export
nhl_game_boxscore <- function(game_id) {
    game_id <- as.integer(game_id)

    base_url <- "https://api-web.nhle.com/v1/gamecenter/"
    full_url <- paste0(base_url, game_id, "/boxscore")

    tryCatch(
        expr = {
            res <- httr::RETRY("GET", full_url)
            check_status(res)

            raw <- httr::content(res, as = "text", encoding = "UTF-8")
            data <- jsonlite::fromJSON(raw, flatten = FALSE)

            # ── game info ─────────────────────────────────────────────────
            game_info <- .boxscore_game_info(data)

            # ── team box ──────────────────────────────────────────────────
            team_box <- .boxscore_team_box(data)

            # ── skater stats ──────────────────────────────────────────────
            skater_stats <- .boxscore_skater_stats(data)

            # ── goalie stats ──────────────────────────────────────────────
            goalie_stats <- .boxscore_goalie_stats(data)

            ts <- Sys.time()
            game <- list(
                game_info = game_info %>%
                    make_fastRhockey_data(
                        "NHL Game Boxscore Info from NHL.com",
                        ts
                    ),
                team_box = team_box %>%
                    make_fastRhockey_data("NHL Game Team Box from NHL.com", ts),
                skater_stats = skater_stats %>%
                    make_fastRhockey_data(
                        "NHL Game Skater Stats from NHL.com",
                        ts
                    ),
                goalie_stats = goalie_stats %>%
                    make_fastRhockey_data(
                        "NHL Game Goalie Stats from NHL.com",
                        ts
                    )
            )

            return(game)
        },
        error = function(e) {
            message(glue::glue("Problem fetching boxscore for game {game_id}"))
            message(e)
        }
    )
}


# ── Helpers ──────────────────────────────────────────────────────────────

#' Build one-row game-info tibble from boxscore response
#' @noRd
.boxscore_game_info <- function(data) {
    venue_name <- tryCatch(
        data$venue$default %||% NA_character_,
        error = function(e) NA_character_
    )

    away <- data$awayTeam
    home <- data$homeTeam

    outcome <- tryCatch(
        data$gameOutcome$lastPeriodType %||% NA_character_,
        error = function(e) NA_character_
    )

    tibble::tibble(
        game_id = as.integer(data$id),
        season = as.integer(data$season),
        game_type = as.integer(data$gameType),
        game_date = as.character(data$gameDate),
        venue = venue_name,
        game_state = as.character(data$gameState %||% NA_character_),
        away_team_id = as.integer(away$id),
        away_team_abbrev = as.character(away$abbrev),
        away_team_name = as.character(
            away$commonName$default %||% NA_character_
        ),
        away_score = as.integer(away$score %||% NA_integer_),
        away_sog = as.integer(away$sog %||% NA_integer_),
        home_team_id = as.integer(home$id),
        home_team_abbrev = as.character(home$abbrev),
        home_team_name = as.character(
            home$commonName$default %||% NA_character_
        ),
        home_score = as.integer(home$score %||% NA_integer_),
        home_sog = as.integer(home$sog %||% NA_integer_),
        last_period_type = outcome
    )
}


#' Build two-row team box tibble (away + home)
#' @noRd
.boxscore_team_box <- function(data) {
    away <- data$awayTeam
    home <- data$homeTeam

    pbs <- data$playerByGameStats

    # Aggregate skater stats per team
    away_sk <- .aggregate_team_skaters(pbs$awayTeam)
    home_sk <- .aggregate_team_skaters(pbs$homeTeam)

    # Aggregate goalie stats per team
    away_g <- .aggregate_team_goalies(pbs$awayTeam)
    home_g <- .aggregate_team_goalies(pbs$homeTeam)

    build_row <- function(team, sk, g, side) {
        tibble::tibble(
            home_away = side,
            team_id = as.integer(team$id),
            team_abbrev = as.character(team$abbrev),
            team_name = as.character(
                team$commonName$default %||% NA_character_
            ),
            goals = as.integer(team$score %||% NA_integer_),
            shots_on_goal = as.integer(team$sog %||% NA_integer_),
            pim = sk$pim,
            hits = sk$hits,
            blocked_shots = sk$blocked_shots,
            giveaways = sk$giveaways,
            takeaways = sk$takeaways,
            power_play_goals = sk$power_play_goals,
            faceoff_win_pctg = sk$faceoff_win_pctg,
            saves = g$saves,
            save_pctg = g$save_pctg,
            goals_against = g$goals_against
        )
    }

    dplyr::bind_rows(
        build_row(away, away_sk, away_g, "away"),
        build_row(home, home_sk, home_g, "home")
    )
}


#' Aggregate skater-level stats to team totals
#' @noRd
.aggregate_team_skaters <- function(team_pbs) {
    skaters <- dplyr::bind_rows(
        .safe_df(team_pbs$forwards),
        .safe_df(team_pbs$defense)
    )

    if (nrow(skaters) == 0L) {
        return(list(
            pim = NA_integer_,
            hits = NA_integer_,
            blocked_shots = NA_integer_,
            giveaways = NA_integer_,
            takeaways = NA_integer_,
            power_play_goals = NA_integer_,
            faceoff_win_pctg = NA_real_
        ))
    }

    list(
        pim = sum(skaters$pim, na.rm = TRUE),
        hits = sum(skaters$hits, na.rm = TRUE),
        blocked_shots = sum(skaters$blockedShots, na.rm = TRUE),
        giveaways = sum(skaters$giveaways, na.rm = TRUE),
        takeaways = sum(skaters$takeaways, na.rm = TRUE),
        power_play_goals = sum(skaters$powerPlayGoals, na.rm = TRUE),
        faceoff_win_pctg = round(
            mean(
                skaters$faceoffWinningPctg[skaters$faceoffWinningPctg > 0],
                na.rm = TRUE
            ),
            4
        )
    )
}


#' Aggregate goalie-level stats to team totals
#' @noRd
.aggregate_team_goalies <- function(team_pbs) {
    goalies <- .safe_df(team_pbs$goalies)

    if (nrow(goalies) == 0L) {
        return(list(
            saves = NA_integer_,
            save_pctg = NA_real_,
            goals_against = NA_integer_
        ))
    }

    list(
        saves = sum(goalies$saves, na.rm = TRUE),
        save_pctg = round(
            sum(goalies$saves, na.rm = TRUE) /
                max(sum(goalies$shotsAgainst, na.rm = TRUE), 1L),
            4
        ),
        goals_against = sum(goalies$goalsAgainst, na.rm = TRUE)
    )
}


#' Extract the "default" language name from a name field
#'
#' The NHL API returns name objects like `{"default": "...", "cs": "...", ...}`.
#' Depending on `flatten` and API shape, `name_field` may be:
#' * a data frame with a `default` column (fromJSON flatten = FALSE)
#' * a character vector (already extracted / flatten = TRUE)
#' * a list of lists
#' @noRd
.extract_default_names <- function(name_field) {
    if (is.data.frame(name_field)) {
        return(as.character(name_field$default))
    }
    if (is.character(name_field)) {
        return(name_field)
    }
    if (is.list(name_field)) {
        return(vapply(
            name_field,
            function(n) {
                if (is.list(n)) {
                    n$default %||% NA_character_
                } else {
                    as.character(n)
                }
            },
            character(1)
        ))
    }
    as.character(name_field)
}


#' Build skater stats tibble (forwards + defense, both teams)
#' @noRd
.boxscore_skater_stats <- function(data) {
    pbs <- data$playerByGameStats

    parse_side <- function(team_pbs, team_data, side) {
        fwd <- .safe_df(team_pbs$forwards)
        def <- .safe_df(team_pbs$defense)
        sk <- dplyr::bind_rows(fwd, def)

        if (nrow(sk) == 0L) {
            return(tibble::tibble())
        }

        tibble::tibble(
            home_away = side,
            team_id = as.integer(team_data$id),
            team_abbrev = as.character(team_data$abbrev),
            player_id = as.integer(sk$playerId),
            player_name = .extract_default_names(sk$name),
            sweater_number = as.integer(sk$sweaterNumber),
            position = as.character(sk$position),
            goals = as.integer(sk$goals),
            assists = as.integer(sk$assists),
            points = as.integer(sk$points),
            plus_minus = as.integer(sk$plusMinus),
            pim = as.integer(sk$pim),
            hits = as.integer(sk$hits),
            power_play_goals = as.integer(sk$powerPlayGoals),
            shots_on_goal = as.integer(sk$sog),
            faceoff_winning_pctg = as.numeric(sk$faceoffWinningPctg),
            toi = as.character(sk$toi),
            blocked_shots = as.integer(sk$blockedShots),
            shifts = as.integer(sk$shifts),
            giveaways = as.integer(sk$giveaways),
            takeaways = as.integer(sk$takeaways)
        )
    }

    dplyr::bind_rows(
        parse_side(pbs$awayTeam, data$awayTeam, "away"),
        parse_side(pbs$homeTeam, data$homeTeam, "home")
    )
}


#' Build goalie stats tibble (both teams)
#' @noRd
.boxscore_goalie_stats <- function(data) {
    pbs <- data$playerByGameStats

    parse_side <- function(team_pbs, team_data, side) {
        gl <- .safe_df(team_pbs$goalies)
        if (nrow(gl) == 0L) {
            return(tibble::tibble())
        }

        tibble::tibble(
            home_away = side,
            team_id = as.integer(team_data$id),
            team_abbrev = as.character(team_data$abbrev),
            player_id = as.integer(gl$playerId),
            player_name = .extract_default_names(gl$name),
            sweater_number = as.integer(gl$sweaterNumber),
            even_strength_shots_against = as.character(
                gl$evenStrengthShotsAgainst
            ),
            power_play_shots_against = as.character(gl$powerPlayShotsAgainst),
            shorthanded_shots_against = as.character(
                gl$shorthandedShotsAgainst
            ),
            save_shots_against = as.character(gl$saveShotsAgainst),
            save_pctg = as.numeric(gl$savePctg),
            even_strength_goals_against = as.integer(
                gl$evenStrengthGoalsAgainst
            ),
            power_play_goals_against = as.integer(gl$powerPlayGoalsAgainst),
            shorthanded_goals_against = as.integer(gl$shorthandedGoalsAgainst),
            pim = as.integer(gl$pim),
            goals_against = as.integer(gl$goalsAgainst),
            toi = as.character(gl$toi),
            starter = as.logical(gl$starter),
            decision = as.character(gl$decision %||% NA_character_),
            shots_against = as.integer(gl$shotsAgainst),
            saves = as.integer(gl$saves)
        )
    }

    dplyr::bind_rows(
        parse_side(pbs$awayTeam, data$awayTeam, "away"),
        parse_side(pbs$homeTeam, data$homeTeam, "home")
    )
}


#' Safely coerce to data frame — returns zero-row tibble on NULL/error
#' @noRd
.safe_df <- function(x) {
    if (is.null(x)) {
        return(tibble::tibble())
    }
    tryCatch(
        tibble::as_tibble(x),
        error = function(e) tibble::tibble()
    )
}
