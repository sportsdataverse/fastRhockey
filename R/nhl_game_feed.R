#' @title **NHL Game Feed**
#' @description Returns information on game feed for a given game id
#' @param game_id Game unique ID
#' @return Returns a named list of data frames: all_plays, scoring_plays,
#' penalty_plays, plays_by_period, current_play, linescore, decisions,
#' team_box, player_box, skaters, goalies, on_ice, on_ice_plus,
#' penalty_box, scratches, team_coaches
#' @keywords NHL Game Feed
#' @import rvest
#' @importFrom rlang .data
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr mutate filter select rename bind_cols bind_rows
#' @importFrom tidyr unnest unnest_wider everything
#' @importFrom janitor clean_names
#' @export
#' @examples
#' \donttest{
#'    try(nhl_game_feed(game_id=2018020561))
#' }
nhl_game_feed <- function(game_id){

  base_url <- "https://statsapi.web.nhl.com/api/v1/game/"

  full_url <- paste0(base_url,
                     game_id,
                     "/feed/live")


  res <- httr::RETRY("GET", full_url)

  # Check the result
  check_status(res)

  tryCatch(
    expr = {
      resp <- res %>%
        httr::content(as = "text", encoding = "UTF-8")
      #####----- Game Rosters ----
      players <- jsonlite::fromJSON(resp)[["gameData"]]
      players_df <- players %>%
        purrr::pluck("players") %>%
        dplyr::tibble() %>%
        tidyr::unnest_wider(".")
      players_df <- players_df %>%
        dplyr::rename(
          "player_id" = "id",
          "player_fullName" = "fullName",
          "player_link" = "link",
          "player_firstName" = "firstName",
          "player_lastName" = "lastName",
          "player_primaryNumber" = "primaryNumber",
          "player_birthDate" = "birthDate",
          "player_currentAge" = "currentAge",
          "player_birthCity" = "birthCity",
          "player_birthStateProvince" = "birthStateProvince",
          "player_birthCountry" = "birthCountry",
          "player_nationality" = "nationality",
          "player_height" = "height",
          "player_weight" = "weight"
        )

      players_df <- players_df %>%
        tidyr::unnest_wider("currentTeam", names_sep = "_") %>%
        tidyr::unnest_wider("primaryPosition", names_sep = "_")
      rosters <- players_df %>%
        dplyr::mutate(
          priority = ifelse(.data$primaryPosition_abbreviation == "G", 2, 1)) %>%
        dplyr::arrange(.data$priority) %>%
        dplyr::select(
          "player_id" = "player_id",
          "player_name" = "player_fullName",
          "position_type" = "primaryPosition_type",
          "position" = "primaryPosition_abbreviation")

      ###--- Game Info ----
      gameInfo <- jsonlite::fromJSON(resp)[["gameData"]]
      game <- gameInfo$game %>%
        dplyr::bind_rows() %>%
        dplyr::rename(
          "game_id" = "pk",
          "season_type" = "type"
        )

      datetime <- gameInfo$datetime %>%
        dplyr::bind_rows()

      # older seasons don't include end time of game

      if("endDateTime" %in% names(datetime)){
        datetime <- datetime %>%
          dplyr::mutate(
            game_start = .data$dateTime %>%
              lubridate::parse_date_time("ymd_HMS") %>%
              lubridate::with_tz("US/Eastern"),
            game_end = .data$endDateTime %>%
              lubridate::parse_date_time("ymd_HMS") %>%
              lubridate::with_tz("US/Eastern"),
            game_date = lubridate::date(.data$game_start),
            game_length = lubridate::as.period(.data$game_end - .data$game_start)
          ) %>%
          dplyr::select("game_date", "game_start", "game_end", "game_length")
      } else {
        datetime <- datetime %>%
          dplyr::mutate(
            game_start = .data$dateTime %>%
              lubridate::parse_date_time("ymd_HMS") %>%
              lubridate::with_tz("US/Eastern"),
            game_date = lubridate::date(.data$game_start),
          ) %>%
          dplyr::select("game_date", "game_start")
      }

      status <- gameInfo[["status"]] %>%
        dplyr::bind_rows() %>%
        dplyr::select("game_state" = "abstractGameState", "detailed_state" = "detailedState")

      venue <- gameInfo[["venue"]] %>%
        dplyr::bind_rows()

      colnames(venue) <- glue::glue("venue_{names(venue)}")

      team_names_keep <- c(
        "home_name","home_abbreviation","home_division_name","home_conference_name","home_id",
        "away_name","away_abbreviation","away_division_name","away_conference_name","away_id"
      )

      teams <- gameInfo$teams %>%
        unlist() %>%
        dplyr::bind_rows() %>%
        janitor::clean_names() %>%
        dplyr::select(dplyr::matches(team_names_keep))

      game_info <- dplyr::bind_cols(
        game, datetime, status, venue, teams
      )
      #---Live Data----
      corsi_events <- c("MISSED_SHOT","SHOT","GOAL","BLOCKED_SHOT")
      fenwick_events <- c("MISSED_SHOT","SHOT","GOAL")
      live_data_df <- jsonlite::fromJSON(resp)[["liveData"]]
      plays_df <- live_data_df$plays$allPlays
      plays_player <- jsonlite::fromJSON(jsonlite::toJSON(plays_df$players), flatten = TRUE)
      # plays_df <- jsonlite::fromJSON(jsonlite::toJSON(plays_df),flatten=TRUE)
      if (length(plays_player) > 0) {
        plays_player_df <- purrr::map_dfr(1:length(plays_player), function(x){
          if("playerType" %in% colnames(plays_player[[x]]) && nrow(plays_player[[x]]) == 4){

            player <- data.frame(
              "event_player_1_type" = ifelse(!is.null(plays_player[[x]]$playerType[[1]]), plays_player[[x]]$playerType[[1]], NA_character_),
              "event_player_1_id" = ifelse(!is.null(plays_player[[x]]$player.id[[1]]),plays_player[[x]]$player.id[[1]], NA_integer_),
              "event_player_1_name" = ifelse(!is.null(plays_player[[x]]$player.fullName[[1]]), plays_player[[x]]$player.fullName[[1]], NA_character_),
              "event_player_1_link" = ifelse(!is.null(plays_player[[x]]$player.link[[1]]), plays_player[[x]]$player.link[[1]], NA_character_),
              "event_player_2_type" = ifelse(!is.null(plays_player[[x]]$playerType[[2]]), plays_player[[x]]$playerType[[2]], NA_character_),
              "event_player_2_id" = ifelse(!is.null(plays_player[[x]]$player.id[[2]]),plays_player[[x]]$player.id[[2]], NA_integer_),
              "event_player_2_name" = ifelse(!is.null(plays_player[[x]]$player.fullName[[2]]), plays_player[[x]]$player.fullName[[2]], NA_character_),
              "event_player_2_link" = ifelse(!is.null(plays_player[[x]]$player.link[[2]]), plays_player[[x]]$player.link[[2]], NA_character_),
              "event_player_3_type" = ifelse(!is.null(plays_player[[x]]$playerType[[3]]), plays_player[[x]]$playerType[[3]], NA_character_),
              "event_player_3_id" = ifelse(!is.null(plays_player[[x]]$player.id[[3]]),plays_player[[x]]$player.id[[3]], NA_integer_),
              "event_player_3_name" = ifelse(!is.null(plays_player[[x]]$player.fullName[[3]]), plays_player[[x]]$player.fullName[[3]], NA_character_),
              "event_player_3_link" = ifelse(!is.null(plays_player[[x]]$player.link[[3]]), plays_player[[x]]$player.link[[3]], NA_character_),
              "event_player_4_type" = ifelse(!is.null(plays_player[[x]]$playerType[[4]]), plays_player[[x]]$playerType[[4]], NA_character_),
              "event_player_4_id" = ifelse(!is.null(plays_player[[x]]$player.id[[4]]),plays_player[[x]]$player.id[[4]], NA_integer_),
              "event_player_4_name" = ifelse(!is.null(plays_player[[x]]$player.fullName[[4]]), plays_player[[x]]$player.fullName[[4]], NA_character_),
              "event_player_4_link" = ifelse(!is.null(plays_player[[x]]$player.link[[4]]), plays_player[[x]]$player.link[[4]], NA_character_))
            return(player)
          }else if("playerType" %in% colnames(plays_player[[x]]) && nrow(plays_player[[x]]) == 3){

            player <- data.frame(
              "event_player_1_type" = ifelse(!is.null(plays_player[[x]]$playerType[[1]]), plays_player[[x]]$playerType[[1]], NA_character_),
              "event_player_1_id" = ifelse(!is.null(plays_player[[x]]$player.id[[1]]),plays_player[[x]]$player.id[[1]], NA_integer_),
              "event_player_1_name" = ifelse(!is.null(plays_player[[x]]$player.fullName[[1]]), plays_player[[x]]$player.fullName[[1]], NA_character_),
              "event_player_1_link" = ifelse(!is.null(plays_player[[x]]$player.link[[1]]), plays_player[[x]]$player.link[[1]], NA_character_),
              "event_player_2_type" = ifelse(!is.null(plays_player[[x]]$playerType[[2]]), plays_player[[x]]$playerType[[2]], NA_character_),
              "event_player_2_id" = ifelse(!is.null(plays_player[[x]]$player.id[[2]]),plays_player[[x]]$player.id[[2]], NA_integer_),
              "event_player_2_name" = ifelse(!is.null(plays_player[[x]]$player.fullName[[2]]), plays_player[[x]]$player.fullName[[2]], NA_character_),
              "event_player_2_link" = ifelse(!is.null(plays_player[[x]]$player.link[[2]]), plays_player[[x]]$player.link[[2]], NA_character_),
              "event_player_3_type" = ifelse(!is.null(plays_player[[x]]$playerType[[3]]), plays_player[[x]]$playerType[[3]], NA_character_),
              "event_player_3_id" = ifelse(!is.null(plays_player[[x]]$player.id[[3]]),plays_player[[x]]$player.id[[3]], NA_integer_),
              "event_player_3_name" = ifelse(!is.null(plays_player[[x]]$player.fullName[[3]]), plays_player[[x]]$player.fullName[[3]], NA_character_),
              "event_player_3_link" = ifelse(!is.null(plays_player[[x]]$player.link[[3]]), plays_player[[x]]$player.link[[3]], NA_character_),
              "event_player_4_type" = NA_character_,
              "event_player_4_id" = NA_integer_,
              "event_player_4_name" = NA_character_,
              "event_player_4_link" = NA_character_)
            return(player)
          }else if("playerType" %in% colnames(plays_player[[x]]) && nrow(plays_player[[x]]) == 2){

            player <- data.frame(
              "event_player_1_type" = ifelse(!is.null(plays_player[[x]]$playerType[[1]]), plays_player[[x]]$playerType[[1]], NA_character_),
              "event_player_1_id" = ifelse(!is.null(plays_player[[x]]$player.id[[1]]),plays_player[[x]]$player.id[[1]], NA_integer_),
              "event_player_1_name" = ifelse(!is.null(plays_player[[x]]$player.fullName[[1]]), plays_player[[x]]$player.fullName[[1]], NA_character_),
              "event_player_1_link" = ifelse(!is.null(plays_player[[x]]$player.link[[1]]), plays_player[[x]]$player.link[[1]], NA_character_),
              "event_player_2_type" = ifelse(!is.null(plays_player[[x]]$playerType[[2]]), plays_player[[x]]$playerType[[2]], NA_character_),
              "event_player_2_id" = ifelse(!is.null(plays_player[[x]]$player.id[[2]]),plays_player[[x]]$player.id[[2]], NA_integer_),
              "event_player_2_name" = ifelse(!is.null(plays_player[[x]]$player.fullName[[2]]), plays_player[[x]]$player.fullName[[2]], NA_character_),
              "event_player_2_link" = ifelse(!is.null(plays_player[[x]]$player.link[[2]]), plays_player[[x]]$player.link[[2]], NA_character_),
              "event_player_3_type" = NA_character_,
              "event_player_3_id" = NA_integer_,
              "event_player_3_name" = NA_character_,
              "event_player_3_link" = NA_character_,
              "event_player_4_type" = NA_character_,
              "event_player_4_id" = NA_integer_,
              "event_player_4_name" = NA_character_,
              "event_player_4_link" = NA_character_)
            return(player)
          }else if("playerType" %in% colnames(plays_player[[x]]) && nrow(plays_player[[x]]) == 1){

            player <- data.frame(
              "event_player_1_type" = ifelse(!is.null(plays_player[[x]]$playerType[[1]]), plays_player[[x]]$playerType[[1]], NA_character_),
              "event_player_1_id" = ifelse(!is.null(plays_player[[x]]$player.id[[1]]),plays_player[[x]]$player.id[[1]], NA_integer_),
              "event_player_1_name" = ifelse(!is.null(plays_player[[x]]$player.fullName[[1]]), plays_player[[x]]$player.fullName[[1]], NA_character_),
              "event_player_1_link" = ifelse(!is.null(plays_player[[x]]$player.link[[1]]), plays_player[[x]]$player.link[[1]], NA_character_),
              "event_player_2_type" = NA_character_,
              "event_player_2_id" = NA_integer_,
              "event_player_2_name" = NA_character_,
              "event_player_2_link" = NA_character_,
              "event_player_3_type" = NA_character_,
              "event_player_3_id" = NA_integer_,
              "event_player_3_name" = NA_character_,
              "event_player_3_link" = NA_character_,
              "event_player_4_type" = NA_character_,
              "event_player_4_id" = NA_integer_,
              "event_player_4_name" = NA_character_,
              "event_player_4_link" = NA_character_)
            return(player)
          }else{
            player <- data.frame(
              "event_player_1_type" = NA_character_,
              "event_player_1_id" = NA_integer_,
              "event_player_1_name" = NA_character_,
              "event_player_1_link" = NA_character_,
              "event_player_2_type" = NA_character_,
              "event_player_2_id" = NA_integer_,
              "event_player_2_name" = NA_character_,
              "event_player_2_link" = NA_character_,
              "event_player_3_type" = NA_character_,
              "event_player_3_id" = NA_integer_,
              "event_player_3_name" = NA_character_,
              "event_player_3_link" = NA_character_,
              "event_player_4_type" = NA_character_,
              "event_player_4_id" = NA_integer_,
              "event_player_4_name" = NA_character_,
              "event_player_4_link" = NA_character_)
            return(player)
          }
        })
      }
      if (length(plays_df) > 0 && length(plays_player) > 0) {
        plays_df <- live_data_df$plays$allPlays
        all_plays <- plays_df %>%
          dplyr::tibble() %>%
          tidyr::unnest_wider("result") %>%
          dplyr::select(-"players")
        if ("strength" %in% names(all_plays)) {
          all_plays <- all_plays %>%
            tidyr::unnest_wider("strength") %>%
            dplyr::rename(
              "strength_code" = "code",
              "strength" = "name")
        }
        all_plays <- all_plays  %>%
          tidyr::unnest_wider("about") %>%
          tidyr::unnest_wider("goals") %>%
          dplyr::rename(
            "home_score" = "home",
            "away_score" = "away")
        if (!is.null(suppressWarnings(all_plays$coordinates))) {
          all_plays <- all_plays %>%
            tidyr::unnest_wider("coordinates")
        }
        all_plays <- all_plays %>%
          tidyr::unnest_wider("team")
        if("triCode" %in% names(all_plays)){
          all_plays <- all_plays %>%
            dplyr::rename(
              "event_team" = "name",
              "event_team_id" = "id",
              "event_team_link" = "link",
              "event_team_abbr" = "triCode"
            )
        } else {
          all_plays <- all_plays %>%
            dplyr::rename(
              "event_team" = "name",
              "event_team_id" = "id",
              "event_team_link" = "link") %>%
            dplyr::mutate(
              event_team_abbr = ifelse(.data$event_team == game_info$home_name,
                                       game_info$home_abbreviation,
                                       game_info$away_abbreviation))
        }

        all_plays <- all_plays %>%
          janitor::clean_names() %>%
          dplyr::mutate(
            # clean up the times
            period_seconds = lubridate::period_to_seconds(lubridate::ms(.data$period_time)),
            game_seconds = .data$period_seconds + (1200 * (.data$period-1)),
            period_seconds_remaining = lubridate::period_to_seconds(lubridate::ms(.data$period_time_remaining)),
            game_seconds_remaining = ifelse(
              .data$period < 4,
              ((3-.data$period) * 1200) + .data$period_seconds_remaining,
              0 - .data$period_seconds),
            home_final = dplyr::last(.data$home_score),
            away_final = dplyr::last(.data$away_score)) %>%
          dplyr::rename("event_type" = "event_type_id")

        # add event players
        players <- plays_player_df
        # combine it all
        pbp <- dplyr::bind_cols(all_plays, players, game_info) %>%
          dplyr::select(1:5,dplyr::all_of(names(players)),dplyr::everything()) %>%
          dplyr::filter(
            # discard redundant rows
            !(.data$event_type %in% c("PERIOD_READY","PERIOD_OFFICIAL","PERIOD_START","GAME_OFFICIAL"))
          )

        # create dummy secondary_type column for preseason/all-star games without one
        if (!("secondary_type" %in% names(pbp))) {
          pbp <- pbp %>%
            dplyr::mutate(secondary_type = NA_character_)
        }

        # create dummy penalty column for preseason/all-star games without one
        if (!("penalty_severity" %in% names(pbp))) {
          pbp <- pbp %>%
            dplyr::mutate(penalty_severity = NA_character_)
        }

        # swap blocked shot event players so
        # shooter is player_1 & blocker is player_2
        if("BLOCKED_SHOT" %in% pbp$event_type){
          pbp_blocks <- pbp %>%
            dplyr::filter(.data$event_type == "BLOCKED_SHOT") %>%
            dplyr::mutate(
              # swap event team to match shooting team instead of blocking team
              event_team = ifelse(.data$event_team == .data$home_name, .data$away_name, .data$home_name),
              event_team_abbr = ifelse(.data$event_team == .data$home_name, .data$home_abbreviation, .data$away_abbreviation),
              event_team_id = ifelse(.data$event_team == .data$home_name, as.integer(.data$home_id), as.integer(.data$away_id)),
              event_team_link = glue::glue("/api/v1/teams/{.data$event_team_id}"),
              blocker_info = glue::glue(
                "{.data$event_player_1_id},{.data$event_player_1_name},{.data$event_player_1_link},{.data$event_player_1_type}"
              ),
              event_player_1_id = .data$event_player_2_id,
              event_player_1_name = .data$event_player_2_name,
              event_player_1_link = .data$event_player_2_link,
              event_player_1_type = .data$event_player_2_type
            ) %>%
            tidyr::separate(
              "blocker_info",
              into = c("event_player_2_id","event_player_2_name",
                       "event_player_2_link","event_player_2_type"),
              sep = ",", remove = TRUE
            ) %>%
            dplyr::mutate(event_player_2_id = as.integer(.data$event_player_2_id))

          pbp <- pbp %>%
            dplyr::filter(.data$event_type != "BLOCKED_SHOT") %>%
            dplyr::bind_rows(pbp_blocks) %>%
            dplyr::arrange(.data$event_idx)
        }
        # replace player_4 with goalie
        # though sometimes there is no player_4
        #   ie if only one goal was scored and it's unassisted,
        #   there will only be two event players

        if("event_player_4_name" %in% names(pbp)){
          pbp <- pbp %>%
            dplyr::mutate(
              event_goalie_id = dplyr::case_when(
                .data$event_player_2_type == "Goalie" ~ .data$event_player_2_id,
                .data$event_player_3_type == "Goalie" ~ .data$event_player_3_id,
                .data$event_player_4_type == "Goalie" ~ .data$event_player_4_id
              ),
              event_goalie_name = dplyr::case_when(
                .data$event_player_2_type == "Goalie" ~ .data$event_player_2_name,
                .data$event_player_3_type == "Goalie" ~ .data$event_player_3_name,
                .data$event_player_4_type == "Goalie" ~ .data$event_player_4_name
              ),
              event_goalie_link = dplyr::case_when(
                .data$event_player_2_type == "Goalie" ~ .data$event_player_2_link,
                .data$event_player_3_type == "Goalie" ~ .data$event_player_3_link,
                .data$event_player_4_type == "Goalie" ~ .data$event_player_4_link
              ),
              event_goalie_type = dplyr::case_when(
                .data$event_player_2_type == "Goalie" ~ "Goalie",
                .data$event_player_3_type == "Goalie" ~ "Goalie",
                .data$event_player_4_type == "Goalie" ~ "Goalie"
              )
            )
        } else if ("event_player_3_name" %in% names(pbp) & !("event_player_4_name" %in% names(pbp))) {
          pbp <- pbp %>%
            dplyr::mutate(
              event_goalie_id = dplyr::case_when(
                .data$event_player_2_type == "Goalie" ~ .data$event_player_2_id,
                .data$event_player_3_type == "Goalie" ~ .data$event_player_3_id
              ),
              event_goalie_name = dplyr::case_when(
                .data$event_player_2_type == "Goalie" ~ .data$event_player_2_name,
                .data$event_player_3_type == "Goalie" ~ .data$event_player_3_name
              ),
              event_goalie_link = dplyr::case_when(
                .data$event_player_2_type == "Goalie" ~ .data$event_player_2_link,
                .data$event_player_3_type == "Goalie" ~ .data$event_player_3_link
              ),
              event_goalie_type = dplyr::case_when(
                .data$event_player_2_type == "Goalie" ~ "Goalie",
                .data$event_player_3_type == "Goalie" ~ "Goalie"
              )
            )
        } else if (!("event_player_3_name" %in% names(pbp))) {
          pbp <- pbp %>%
            dplyr::mutate(
              event_goalie_id = dplyr::case_when(
                .data$event_player_2_type == "Goalie" ~ .data$event_player_2_id
              ),
              event_goalie_name = dplyr::case_when(
                .data$event_player_2_type == "Goalie" ~ .data$event_player_2_name
              ),
              event_goalie_link = dplyr::case_when(
                .data$event_player_2_type == "Goalie" ~ .data$event_player_2_link
              ),
              event_goalie_type = dplyr::case_when(
                .data$event_player_2_type == "Goalie" ~ "Goalie"
              )
            )
        }

        # add shift events

        shifts <- nhl_game_shifts(game_id)

        # if shift data exists
        if(!is.null(shifts)) {
          # MODIFIED FROM THE SCRAPER BY EVOLVING HOCKEY
          # WHO MODIFIED ORIGINAL CODE BY MANNY PERRY

          # combine shift events with pbp events
          pbp_full <- pbp %>%
            dplyr::bind_rows(shifts) %>%
            dplyr::mutate(
              # arrange all events so shift changes are in the proper spot
              priority =
                1 * (.data$event_type %in% c("TAKEAWAY", "GIVEAWAY", "MISSED_SHOT", "HIT", "SHOT", "BLOCKED_SHOT") & !(.data$period == 5 & .data$season_type == "R")) +
                2 * (.data$event_type == "GOAL" & !(.data$period == 5 & .data$season_type == "R")) +
                3 * (.data$event_type == "STOP" & !(.data$period == 5 & .data$season_type == "R")) +
                4 * (.data$event_type == "PENALTY" & !(.data$period == 5 & .data$season_type == "R")) +
                5 * (.data$event_type == "CHANGE" & !(.data$period == 5 & .data$season_type == "R")) +
                6 * (.data$event_type == "PERIOD_END" & !(.data$period == 5 & .data$season_type == "R")) +
                7 * (.data$event_type == "GAME_END" & !(.data$period == 5 & .data$season_type == "R")) +
                8 * (.data$event_type == "FACEOFF" &  !(.data$period == 5 & .data$season_type == "R"))
            ) %>%
            dplyr::arrange(.data$period, .data$game_seconds, .data$priority) %>%
            dplyr::mutate(
              home_index = as.numeric(cumsum(.data$event_type == "CHANGE" &
                                               .data$event_team == unique(pbp$home_name))),
              away_index = as.numeric(cumsum(.data$event_type == "CHANGE" &
                                               .data$event_team == unique(pbp$away_name))),
            ) %>%
            dplyr::select(-"priority")

          # construct a matrix of player names as columns & event row as rows
          home_skaters <- NULL

          for(i in 1:nrow(rosters)){

            player <- rosters$player_name[i]

            skaters_i <- dplyr::tibble(
              on_ice = cumsum(
                1 * stringr::str_detect(
                  dplyr::filter(pbp_full,
                                .data$event_type == "CHANGE" &
                                  .data$event_team == unique(pbp$home_name))$players_on,
                  player) -
                  1 * stringr::str_detect(
                    dplyr::filter(pbp_full,
                                  .data$event_type == "CHANGE" &
                                    .data$event_team == unique(pbp$home_name))$players_off,
                    player)
              )
            )

            suppressMessages({
              home_skaters <- dplyr::bind_cols(home_skaters, skaters_i)
            })

            rm(skaters_i, player)
          }

          colnames(home_skaters) <- rosters$player_name

          home_skaters <- data.frame(home_skaters)

          # transform into matrix of only players on the ice
          on_home <- which(home_skaters == 1, arr.ind = TRUE) %>%
            data.frame() %>%
            dplyr::group_by(.data$row) %>%
            dplyr::summarize(
              home_on_1 = colnames(home_skaters)[unique(.data$col)[1]],
              home_on_2 = colnames(home_skaters)[unique(.data$col)[2]],
              home_on_3 = colnames(home_skaters)[unique(.data$col)[3]],
              home_on_4 = colnames(home_skaters)[unique(.data$col)[4]],
              home_on_5 = colnames(home_skaters)[unique(.data$col)[5]],
              home_on_6 = colnames(home_skaters)[unique(.data$col)[6]],
              home_on_7 = colnames(home_skaters)[unique(.data$col)[7]]
            )

          away_skaters <- NULL

          for(i in 1:nrow(rosters)){

            player <- rosters$player_name[i]

            skaters_i <- dplyr::tibble(
              on_ice = cumsum(
                1 * stringr::str_detect(
                  dplyr::filter(pbp_full,
                                .data$event_type == "CHANGE" &
                                  .data$event_team == unique(pbp$away_name))$players_on,
                  player) -
                  1 * stringr::str_detect(
                    dplyr::filter(pbp_full,
                                  .data$event_type == "CHANGE" &
                                    .data$event_team == unique(pbp$away_name))$players_off,
                    player)
              )
            )

            suppressMessages({
              away_skaters <- dplyr::bind_cols(away_skaters, skaters_i)
            })

            rm(skaters_i, player)
          }

          colnames(away_skaters) <- rosters$player_name

          away_skaters <- data.frame(away_skaters)

          # transform into matrix of only players on the ice
          on_away <- which(away_skaters == 1, arr.ind = TRUE) %>%
            data.frame() %>%
            dplyr::group_by(.data$row) %>%
            dplyr::summarize(
              away_on_1 = colnames(away_skaters)[unique(.data$col)[1]],
              away_on_2 = colnames(away_skaters)[unique(.data$col)[2]],
              away_on_3 = colnames(away_skaters)[unique(.data$col)[3]],
              away_on_4 = colnames(away_skaters)[unique(.data$col)[4]],
              away_on_5 = colnames(away_skaters)[unique(.data$col)[5]],
              away_on_6 = colnames(away_skaters)[unique(.data$col)[6]],
              away_on_7 = colnames(away_skaters)[unique(.data$col)[7]]
            )

          # define goalies
          goalies <- rosters %>%
            dplyr::filter(.data$position == "G") %>%
            dplyr::mutate(
              player_name = stringr::str_replace(.data$player_name, " ", ".")
            ) %>%
            dplyr::mutate(
              player_name = stringr::str_replace(.data$player_name, "-", ".")
            ) %>%
            dplyr::pull("player_name")

          non_plays <- c("GAME_SCHEDULED","PERIOD_END","GAME_END")

          pbp_full <- pbp_full %>%
            dplyr::left_join(on_home, by = c("home_index" = "row")) %>%
            dplyr::left_join(on_away, by = c("away_index" = "row")) %>%
            # adding game info to shift change events and moving info to the end
            dplyr::select(!dplyr::all_of(names(game_info))) %>%
            dplyr::bind_cols(game_info) %>%
            dplyr::mutate(
              # create goalie on-ice columns
              home_goalie = dplyr::case_when(
                .data$home_on_1 %in% goalies ~ .data$home_on_1,
                .data$home_on_2 %in% goalies ~ .data$home_on_2,
                .data$home_on_3 %in% goalies ~ .data$home_on_3,
                .data$home_on_4 %in% goalies ~ .data$home_on_4,
                .data$home_on_5 %in% goalies ~ .data$home_on_5,
                .data$home_on_6 %in% goalies ~ .data$home_on_6,
                .data$home_on_7 %in% goalies ~ .data$home_on_7
              ),
              away_goalie = dplyr::case_when(
                .data$away_on_1 %in% goalies ~ .data$away_on_1,
                .data$away_on_2 %in% goalies ~ .data$away_on_2,
                .data$away_on_3 %in% goalies ~ .data$away_on_3,
                .data$away_on_4 %in% goalies ~ .data$away_on_4,
                .data$away_on_5 %in% goalies ~ .data$away_on_5,
                .data$away_on_6 %in% goalies ~ .data$away_on_6,
                .data$away_on_7 %in% goalies ~ .data$away_on_7
              ),
              # include only skaters in on-ice columns
              # rosters are ordered such that goalies should always be
              # last on-ice skater
              home_on_1 = ifelse(.data$home_on_1 == .data$home_goalie & !is.na(.data$home_goalie), NA_character_, .data$home_on_1),
              home_on_2 = ifelse(.data$home_on_2 == .data$home_goalie & !is.na(.data$home_goalie), NA_character_, .data$home_on_2),
              home_on_3 = ifelse(.data$home_on_3 == .data$home_goalie & !is.na(.data$home_goalie), NA_character_, .data$home_on_3),
              home_on_4 = ifelse(.data$home_on_4 == .data$home_goalie & !is.na(.data$home_goalie), NA_character_, .data$home_on_4),
              home_on_5 = ifelse(.data$home_on_5 == .data$home_goalie & !is.na(.data$home_goalie), NA_character_, .data$home_on_5),
              home_on_6 = ifelse(.data$home_on_6 == .data$home_goalie & !is.na(.data$home_goalie), NA_character_, .data$home_on_6),
              home_on_7 = ifelse(.data$home_on_7 == .data$home_goalie & !is.na(.data$home_goalie), NA_character_, .data$home_on_7),
              away_on_1 = ifelse(.data$away_on_1 == .data$away_goalie & !is.na(.data$away_goalie), NA_character_, .data$away_on_1),
              away_on_2 = ifelse(.data$away_on_2 == .data$away_goalie & !is.na(.data$away_goalie), NA_character_, .data$away_on_2),
              away_on_3 = ifelse(.data$away_on_3 == .data$away_goalie & !is.na(.data$away_goalie), NA_character_, .data$away_on_3),
              away_on_4 = ifelse(.data$away_on_4 == .data$away_goalie & !is.na(.data$away_goalie), NA_character_, .data$away_on_4),
              away_on_5 = ifelse(.data$away_on_5 == .data$away_goalie & !is.na(.data$away_goalie), NA_character_, .data$away_on_5),
              away_on_6 = ifelse(.data$away_on_6 == .data$away_goalie & !is.na(.data$away_goalie), NA_character_, .data$away_on_6),
              away_on_7 = ifelse(.data$away_on_7 == .data$away_goalie & !is.na(.data$away_goalie), NA_character_, .data$away_on_7),
              # create strength states
              home_skaters =
                1 * (!is.na(.data$home_on_1)) + 1 * (!is.na(.data$home_on_2)) +
                1 * (!is.na(.data$home_on_3)) + 1 * (!is.na(.data$home_on_4)) +
                1 * (!is.na(.data$home_on_5)) + 1 * (!is.na(.data$home_on_6)) +
                1 * (!is.na(.data$home_on_7)),
              away_skaters =
                1 * (!is.na(.data$away_on_1)) + 1 * (!is.na(.data$away_on_2)) +
                1 * (!is.na(.data$away_on_3)) + 1 * (!is.na(.data$away_on_4)) +
                1 * (!is.na(.data$away_on_5)) + 1 * (!is.na(.data$away_on_6)) +
                1 * (!is.na(.data$away_on_7)),
              strength_state = dplyr::case_when(
                .data$event_team == .data$home_name ~ glue::glue("{.data$home_skaters}v{.data$away_skaters}"),
                .data$event_team == .data$away_name ~ glue::glue("{.data$away_skaters}v{.data$home_skaters}"),
                TRUE ~ glue::glue("{.data$home_skaters}v{.data$away_skaters}")
              ),
              strength_code = dplyr::case_when(
                .data$home_skaters == .data$away_skaters ~ "EV",
                (.data$home_skaters < .data$away_skaters & .data$event_team == .data$home_name) |
                  (.data$away_skaters < .data$home_skaters & .data$event_team == .data$away_name) ~ "SH",
                (.data$home_skaters < .data$away_skaters & .data$event_team == .data$away_name) |
                  (.data$away_skaters < .data$home_skaters & .data$event_team == .data$home_name) ~ "PP"
              ),
              # fixing the change events at start and end of periods
              strength_code = ifelse(
                .data$event_type %in% non_plays |
                  dplyr::lead(.data$event_type) %in% non_plays |
                  dplyr::lead(dplyr::lead(.data$event_type)) %in% non_plays |
                  dplyr::lag(.data$event_type) %in% non_plays |
                  dplyr::lag(dplyr::lag(.data$event_type)) %in% non_plays,
                NA_character_, .data$strength_code
              ),
              strength = dplyr::case_when(
                .data$strength_code == "EV" ~ "Even",
                .data$strength_code == "SH" ~ "Shorthanded",
                .data$strength_code == "PP" ~ "Power Play"
              ),
              extra_attacker = ifelse(
                ((.data$event_team == .data$home_name & is.na(.data$home_goalie)) |
                   (.data$event_team == .data$away_name & is.na(.data$away_goalie))) &
                  !(.data$event_type %in% non_plays) & .data$event_type != "CHANGE", TRUE, FALSE
              )
            )

          pbp_full <- pbp_full %>%
            dplyr::mutate(
              event_idx = dplyr::row_number()-1,
              home_final = dplyr::last(.data$home_score),
              away_final = dplyr::last(.data$away_score),
              period_seconds_remaining = dplyr::case_when(
                (.data$season_type != "P" & .data$period < 4) | .data$season_type == "P" ~ 1200 - .data$period_seconds,
                .data$season_type != "P" & .data$period == 4 ~ 300 - .data$period_seconds,
                TRUE ~ .data$period_seconds_remaining
              ),
              description = ifelse(
                .data$event_type == "CHANGE",
                glue::glue("ON: {.data$players_on}; OFF: {.data$players_off}"),
                .data$description
              ),
              secondary_type = ifelse(
                .data$event_type == "CHANGE",
                "On the fly",
                .data$secondary_type
              ),
              secondary_type = ifelse(
                .data$event_type == "CHANGE" &
                  (dplyr::lead(.data$event_type) == "FACEOFF" |
                     dplyr::lag(.data$event_type) == "STOP" |
                     dplyr::lag(.data$event_type) == "GOAL" |
                     dplyr::lead(.data$event_type) == "PERIOD_END" |
                     dplyr::lag(.data$event_type) == "PERIOD_END" |
                     dplyr::lead(dplyr::lead(.data$event_type)) == "PERIOD_END" |
                     dplyr::lag(dplyr::lag(.data$event_type)) == "PERIOD_END"),
                "Line change",
                .data$secondary_type
              )
            ) %>%
            # remove unnecessary columns
            dplyr::select(-"event_id",-"home_index",-"away_index",-"event_code")

          # add fixed x & y coordinates so home team shoots right, away shoots left
          pbp_full <- pbp_full %>%
            dplyr::group_by(.data$event_team, .data$period, .data$game_id) %>%
            # find median x shot coordinate to tell us which side teams are shooting on
            dplyr::mutate(med_x = stats::median(.data$x[.data$event_type %in% fenwick_events], na.rm = TRUE)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(
              x_fixed = dplyr::case_when(
                .data$event_team == .data$home_name & .data$med_x > 0 ~ .data$x,
                .data$event_team == .data$home_name & .data$med_x < 0 ~ 0 - .data$x,
                .data$event_team == .data$away_name & .data$med_x > 0 ~ 0 - .data$x,
                .data$event_team == .data$away_name & .data$med_x < 0 ~ .data$x
              ),
              y_fixed = dplyr::case_when(
                .data$event_team == .data$home_name & .data$med_x > 0 ~ .data$y,
                .data$event_team == .data$home_name & .data$med_x < 0 ~ 0 - .data$y,
                .data$event_team == .data$away_name & .data$med_x > 0 ~ 0 - .data$y,
                .data$event_team == .data$away_name & .data$med_x < 0 ~ .data$y
              ),
              # add shot distance/angle
              shot_distance = dplyr::case_when(
                .data$event_team == .data$home_name & .data$event_type %in% fenwick_events ~
                  round(abs(sqrt((.data$x_fixed - 89)^2 + (.data$y_fixed)^2)),1),
                .data$event_team == .data$away_name & .data$event_type %in% fenwick_events ~
                  round(abs(sqrt((.data$x_fixed - (-89))^2 + (.data$y_fixed)^2)),1)
              ),
              shot_angle = dplyr::case_when(
                .data$event_team == .data$home_name & .data$event_type %in% fenwick_events ~
                  round(abs(atan((0-.data$y_fixed) / (89-.data$x_fixed)) * (180 / pi)),1),
                .data$event_team == .data$away_name & .data$event_type %in% fenwick_events ~
                  round(abs(atan((0-.data$y_fixed) / (-89-.data$x_fixed)) * (180 / pi)),1)
              ),
              # fix behind the net angles
              shot_angle = ifelse(
                (.data$event_team == .data$home_name & .data$x_fixed > 89) |
                  (.data$event_team == .data$away_name & .data$x_fixed < -89),
                180 - .data$shot_angle,
                .data$shot_angle
              ),
              event_team_type =  dplyr::case_when(
                .data$event_team == .data$home_name ~ "home",
                .data$event_team == .data$away_name ~ "away"
              )
            ) %>%
            dplyr::select(-"med_x")

          # reorder the columns
          if("event_player_3_name" %in% names(pbp_full)){
            pbp_full <- pbp_full %>%
              # change event player names to match on-ice player name conventions
              dplyr::mutate_at(c("event_player_1_name","event_player_2_name",
                                 "event_player_3_name","event_goalie_name"),
                               ~stringr::str_replace_all(.x, c(" " = ".", "-" = "."))) %>%
              dplyr::select(
                "event_type",
                "event",
                "secondary_type",
                "event_team",
                "event_team_type",
                "description",
                "period",
                "period_seconds",
                "period_seconds_remaining",
                "game_seconds",
                "game_seconds_remaining",
                "home_score",
                "away_score",
                "event_player_1_name",
                "event_player_1_type",
                "event_player_2_name",
                "event_player_2_type",
                "event_player_3_name",
                "event_player_3_type",
                "event_goalie_name",
                "strength_state",
                "strength_code":"event_idx",
                "num_on",
                "players_on",
                "num_off",
                "players_off",
                "extra_attacker",
                "x",
                "y",
                "x_fixed",
                "y_fixed",
                "shot_distance",
                "shot_angle",
                "home_skaters",
                "away_skaters",
                "home_on_1":"away_on_7",
                "home_goalie",
                "away_goalie",
                "game_id",
                "event_idx",
                dplyr::everything()
              )
          } else {
            pbp_full <- pbp_full %>%
              dplyr::mutate_at(c("event_player_1_name","event_player_2_name",
                                 "event_goalie_name"),
                               ~stringr::str_replace_all(.x, c(" " = ".", "-" = "."))) %>%
              dplyr::select(
                "event_type",
                "event",
                "secondary_type",
                "event_team",
                "event_team_type",
                "description",
                "period",
                "period_seconds",
                "period_seconds_remaining",
                "game_seconds",
                "game_seconds_remaining",
                "home_score",
                "away_score",
                "event_player_1_name",
                "event_player_1_type",
                "event_player_2_name",
                "event_player_2_type",
                "event_goalie_name",
                "strength_state",
                "strength_code":"event_idx",
                "num_on",
                "players_on",
                "num_off",
                "players_off",
                "extra_attacker",
                "x",
                "y",
                "x_fixed",
                "y_fixed",
                "shot_distance",
                "shot_angle",
                "home_skaters",
                "away_skaters",
                "home_on_1":"away_on_7",
                "home_goalie",
                "away_goalie",
                "game_id",
                "event_idx",
                dplyr::everything()
              )
          }


          # fill in current score for shift events
          #   first assign score of 0 to start of game to fix issue with
          #   game ID 2018020965, which is missing the "GAME_SCHEDULED" event
          pbp_full$home_score[1] <- 0
          pbp_full$away_score[1] <- 0
          pbp_full <- pbp_full %>%
            tidyr::fill("home_score", .direction = "up") %>%
            tidyr::fill("away_score", .direction = "up")

          # fix period type for shift events
          pbp_full <- pbp_full %>%
            dplyr::mutate(
              period_type = dplyr::case_when(
                .data$period < 4 ~ "REGULAR",
                .data$season_type %in% c("R","PR") & .data$period == 4 ~ "OVERTIME",
                .data$season_type %in% c("R","PR") & .data$period == 5 ~ "SHOOTOUT",
                .data$season_type == "P" & .data$period > 3 ~ "OVERTIME"
              ),
              ordinal_num = dplyr::case_when(
                .data$period == 1 ~ glue::glue("{.data$period}st"),
                .data$period == 2 ~ glue::glue("{.data$period}nd"),
                .data$period == 3 ~ glue::glue("{.data$period}rd"),
                TRUE ~ glue::glue("{.data$period}th")
              )
            )

        } else if(is.null(shifts) & "x" %in% names(pbp)){

          # no shift data available but shot location available
          # ie preseason

          pbp_full <- pbp %>%
            dplyr::select(-"event_id",-"event_code") %>%
            # add fixed x & y coordinates so home team shoots right, away shoots left
            dplyr::group_by(.data$event_team, .data$period, .data$game_id) %>%
            # find median x shot coordinate to tell us which side teams are shooting on
            dplyr::mutate(med_x = stats::median(.data$x[.data$event_type %in% fenwick_events], na.rm = TRUE)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(
              x_fixed = dplyr::case_when(
                .data$event_team == .data$home_name & .data$med_x > 0 ~ .data$x,
                .data$event_team == .data$home_name & .data$med_x < 0 ~ 0 - .data$x,
                .data$event_team == .data$away_name & .data$med_x > 0 ~ 0 - .data$x,
                .data$event_team == .data$away_name & .data$med_x < 0 ~ .data$x
              ),
              y_fixed = dplyr::case_when(
                "event_team" == "home_name" & "med_x" > 0 ~ "y",
                "event_team" == "home_name" & "med_x" < 0 ~ 0 - "y",
                "event_team" == "away_name" & "med_x" > 0 ~ 0 - "y",
                "event_team" == "away_name" & "med_x" < 0 ~ "y"
              ),
              # add shot distance/angle
              shot_distance = dplyr::case_when(
                .data$event_team == .data$home_name & .data$event_type %in% fenwick_events ~
                  round(abs(sqrt((.data$x_fixed - 89)^2 + (.data$y_fixed)^2)),1),
                .data$event_team == .data$away_name & .data$event_type %in% fenwick_events ~
                  round(abs(sqrt((.data$x_fixed - (-89))^2 + (.data$y_fixed)^2)),1)
              ),
              shot_angle = dplyr::case_when(
                .data$event_team == .data$home_name & .data$event_type %in% fenwick_events ~
                  round(abs(atan((0-.data$y_fixed) / (89-.data$x_fixed)) * (180 / pi)),1),
                .data$event_team == .data$away_name & .data$event_type %in% fenwick_events ~
                  round(abs(atan((0-.data$y_fixed) / (-89-.data$x_fixed)) * (180 / pi)),1)
              ),
              # fix behind the net angles
              shot_angle = ifelse(
                (.data$event_team == .data$home_name & .data$x_fixed > 89) |
                  (.data$event_team == .data$away_name & .data$x_fixed < -89),
                180 - .data$shot_angle,
                .data$shot_angle
              ),
              event_team_type =  dplyr::case_when(
                .data$event_team == .data$home_name ~ "home",
                .data$event_team == .data$away_name ~ "away"
              )
            ) %>%
            dplyr::select(-"med_x")

          if("event_player_3_name" %in% names(pbp_full)) {
            pbp_full <- pbp_full %>%
              dplyr::mutate_at(
                c("event_player_1_name","event_player_2_name",
                  "event_player_3_name","event_goalie_name"),
                ~stringr::str_replace_all(.x, c(" " = ".", "-" = "."))
              ) %>%
              dplyr::select(
                "event_type",
                "event",
                "secondary_type",
                "event_team",
                "event_team_type",
                "description",
                "period",
                "period_seconds",
                "period_seconds_remaining",
                "game_seconds",
                "game_seconds_remaining",
                "home_score",
                "away_score",
                "event_player_1_name",
                "event_player_1_type",
                "event_player_2_name",
                "event_player_2_type",
                "event_player_3_name",
                "event_player_3_type",
                "event_goalie_name",
                "penalty_severity":"strength",
                "x",
                "y",
                "x_fixed",
                "y_fixed",
                "shot_distance",
                "shot_angle",
                "game_id",
                "event_idx",
                dplyr::everything()
              )
          } else {
            pbp_full <- pbp_full %>%
              dplyr::mutate_at(
                c("event_player_1_name","event_player_2_name",
                  "event_goalie_name"),
                ~stringr::str_replace_all(.x, c(" " = ".", "-" = "."))
              ) %>%
              dplyr::select(
                "event_type",
                "event",
                "secondary_type",
                "event_team",
                "event_team_type",
                "description",
                "period",
                "period_seconds",
                "period_seconds_remaining",
                "game_seconds",
                "game_seconds_remaining",
                "home_score",
                "away_score",
                "event_player_1_name",
                "event_player_1_type",
                "event_player_2_name",
                "event_player_2_type",
                "event_goalie_name",
                "penalty_severity":"strength",
                "x",
                "y",
                "x_fixed",
                "y_fixed",
                "shot_distance",
                "shot_angle",
                "game_id",
                "event_idx",
                dplyr::everything()
              )
          }

        } else {
          # no shift data or shot location
          if("event_player_3_name" %in% names(pbp)){
            pbp_full <- pbp %>%
              dplyr::mutate_at(
                c("event_player_1_name","event_player_2_name",
                  "event_player_3_name","event_goalie_name"),
                ~stringr::str_replace_all(.x, c(" " = ".", "-" = "."))
              ) %>%
              dplyr::select(-"event_id",-"event_code") %>%
              dplyr::mutate(
                event_team_type =  dplyr::case_when(
                  .data$event_team == .data$home_name ~ "home",
                  .data$event_team == .data$away_name ~ "away"
                )
              ) %>%
              dplyr::select(
                "event_type",
                "event",
                "secondary_type",
                "event_team",
                "event_team_type",
                "description",
                "period",
                "period_time",
                "period_time_remaining",
                "event_player_1_name",
                "event_player_1_type",
                "event_player_2_name",
                "event_player_2_type",
                "event_player_3_name",
                "event_player_3_type",
                "event_goalie_name",
                "game_id",
                "event_idx",
                dplyr::everything()
              )
          } else {
            pbp_full <- pbp %>%
              dplyr::mutate_at(
                c("event_player_1_name","event_player_2_name",
                  "event_goalie_name"),
                ~stringr::str_replace_all(.x, c(" " = ".", "-" = "."))
              ) %>%
              dplyr::select(-"event_id",-"event_code") %>%
              dplyr::mutate(
                event_team_type =  dplyr::case_when(
                  .data$event_team == .data$home_name ~ "home",
                  .data$event_team == .data$away_name ~ "away"
                )
              ) %>%
              dplyr::select(
                "event_type",
                "event",
                "secondary_type",
                "event_team",
                "event_team_type",
                "description",
                "period",
                "period_time",
                "period_time_remaining",
                "event_player_1_name",
                "event_player_1_type",
                "event_player_2_name",
                "event_player_2_type",
                "event_goalie_name",
                "game_id",
                "event_idx",
                dplyr::everything()
              )
          }

        }
        # add event_id
        pbp_full <- pbp_full %>%
          dplyr::mutate(
            event_idx = stringr::str_pad(.data$event_idx, width = 4, side = "left", pad = 0),
            event_id = as.numeric(paste0(.data$game_id,.data$event_idx)),
            secondary_type = ifelse(
              stringr::str_detect(dplyr::lead(.data$description), "PS -") &
                .data$event_type %in% c("SHOT","MISSED_SHOT","GOAL"),
              "Penalty Shot", .data$secondary_type
            )
          )
        pbp_full <- pbp_full %>%
          janitor::clean_names() %>%
          make_fastRhockey_data("NHL Game Plays Information from NHL.com",Sys.time())
      } else {
        pbp_full = data.frame() %>%
          make_fastRhockey_data("NHL Game Plays Information from NHL.com",Sys.time())
      }
      scoring_plays <- live_data_df$plays$scoringPlays %>%
        make_fastRhockey_data("NHL Game Scoring Plays Information from NHL.com",Sys.time())
      penalty_plays <- live_data_df$plays$penaltyPlays %>%
        make_fastRhockey_data("NHL Game Penalty Box Information from NHL.com",Sys.time())
      plays_by_period <- live_data_df$plays$playsByPeriod %>%
        make_fastRhockey_data("NHL Game Plays by Period Information from NHL.com",Sys.time())
      current_play <- live_data_df$plays$currentPlay
      linescore <- live_data_df$linescore

      decisions <- live_data_df$decisions %>%
        make_fastRhockey_data("NHL Game Decisions Information from NHL.com",Sys.time())
      ##-- boxscore ----
      ###---officials----
      officials_df <- live_data_df$boxscore[["officials"]]
      if(length(officials_df) > 1){
        officials_df <- jsonlite::fromJSON(jsonlite::toJSON(officials_df),flatten=TRUE) %>%
          janitor::clean_names() %>%
          make_fastRhockey_data("NHL Game Officials Information from NHL.com",Sys.time())
      }
      game_boxscore_df <- live_data_df$boxscore[["teams"]]
      game_boxscore_df <- jsonlite::fromJSON(jsonlite::toJSON(game_boxscore_df),flatten=TRUE)
      ###---team_box----
      away_boxscore <- game_boxscore_df[["away"]]
      home_boxscore <- game_boxscore_df[["home"]]
      away_team_box <- away_boxscore$team %>%
        dplyr::bind_cols(away_boxscore$teamStats$teamSkaterStats)
      home_team_box <- home_boxscore$team %>%
        dplyr::bind_cols(home_boxscore$teamStats$teamSkaterStats)
      team_box <- dplyr::bind_rows(away_team_box, home_team_box) %>%
        dplyr::rename(
          "team_id" = "id",
          "team_name" = "name") %>%
        janitor::clean_names() %>%
        make_fastRhockey_data("NHL Game Team Box Information from NHL.com",Sys.time())
      ###---player_box----
      away_players_box <- purrr::map_df(1:length(away_boxscore$players),function(x){
        person <- data.frame(away_boxscore$players[[x]][["person"]]) %>%
          janitor::clean_names()
        jersey <- data.frame("jerseyNumber" = away_boxscore$players[[x]][["jerseyNumber"]]) %>%
          janitor::clean_names()
        position <- data.frame(away_boxscore$players[[x]][["position"]]) %>%
          janitor::clean_names()
        player_stats <- data.frame(away_boxscore$players[[x]][["stats"]]) %>%
          janitor::clean_names()
        away_player_boxscore <-dplyr::bind_cols(person,jersey,position,player_stats)
        return(away_player_boxscore)
      })
      home_players_box <- purrr::map_df(1:length(home_boxscore$players),function(x){
        person <- data.frame(home_boxscore$players[[x]][["person"]]) %>%
          janitor::clean_names()
        jersey <- data.frame("jerseyNumber" = home_boxscore$players[[x]][["jerseyNumber"]]) %>%
          janitor::clean_names()
        position <- data.frame(home_boxscore$players[[x]][["position"]]) %>%
          janitor::clean_names()
        player_stats <- data.frame(home_boxscore$players[[x]][["stats"]]) %>%
          janitor::clean_names()
        home_player_boxscore <-dplyr::bind_cols(person,jersey,position,player_stats)
        return(home_player_boxscore)
      })
      away_players_box$home_away <- "Away"
      home_players_box$home_away <- "Home"
      players_box <- dplyr::bind_rows(away_players_box, home_players_box) %>%
        dplyr::rename(
          "player_id" = "id",
          "player_full_name" = "full_name",
          "position_code" = "code",
          "position_name" = "name",
          "position_type" = "type",
          "position_abbreviation" = "abbreviation") %>%
        janitor::clean_names() %>%
        make_fastRhockey_data("NHL Game Players Box Information from NHL.com",Sys.time())

      ###---goalies----
      away_goalies <- data.frame("goalies" = away_boxscore$goalies)
      home_goalies <- data.frame("goalies" = home_boxscore$goalies)
      away_goalies$home_away <- "Away"
      home_goalies$home_away <- "Home"
      goalies <- dplyr::bind_rows(away_goalies,home_goalies) %>%
        make_fastRhockey_data("NHL Game Goalies Information from NHL.com",Sys.time())
      ###---skaters----
      away_skaters <- data.frame("skaters" = away_boxscore$skaters)
      home_skaters <- data.frame("skaters" = home_boxscore$skaters)
      away_skaters$home_away <- "Away"
      home_skaters$home_away <- "Home"
      skaters <- dplyr::bind_rows(away_skaters,home_skaters) %>%
        make_fastRhockey_data("NHL Game Skaters Information from NHL.com",Sys.time())
      ###---onIce----
      away_onIce <- data.frame("onIce" = away_boxscore$onIce)
      home_onIce <- data.frame("onIce" = home_boxscore$onIce)
      onIce <- dplyr::bind_rows(away_onIce,home_onIce) %>%
        make_fastRhockey_data("NHL Game On Ice Information from NHL.com",Sys.time())
      ###---onIcePlus----
      away_onIcePlus <- data.frame("onIcePlus" = away_boxscore$onIcePlus)
      home_onIcePlus <- data.frame("onIcePlus" = home_boxscore$onIcePlus)
      onIcePlus <- dplyr::bind_rows(away_onIcePlus,home_onIcePlus) %>%
        make_fastRhockey_data("NHL Game On Ice+ Information from NHL.com",Sys.time())
      ###---penaltyBox----
      away_penaltyBox <- data.frame("penaltyBox" = away_boxscore$penaltyBox)
      home_penaltyBox <- data.frame("penaltyBox" = home_boxscore$penaltyBox)
      penaltyBox <- dplyr::bind_rows(away_penaltyBox,home_penaltyBox) %>%
        make_fastRhockey_data("NHL Game Penalty Box Information from NHL.com",Sys.time())
      ###---scratches----
      away_scratches <- data.frame("scratches" = away_boxscore$scratches)
      home_scratches <- data.frame("scratches" = home_boxscore$scratches)
      scratches <- dplyr::bind_rows(away_scratches,home_scratches) %>%
        make_fastRhockey_data("NHL Game Scratches Information from NHL.com",Sys.time())
      ###---coaches----
      away_coaches <- away_boxscore$coaches
      home_coaches <- home_boxscore$coaches
      away_coaches$home_away <- "Away"
      home_coaches$home_away <- "Home"
      team_coaches <- dplyr::bind_rows(away_coaches, home_coaches) %>%
        make_fastRhockey_data("NHL Game Team Coaches Information from NHL.com",Sys.time())
      ###---
      game = c(list(pbp_full),list(scoring_plays), list(penalty_plays),list(plays_by_period),
               list(current_play), list(linescore), list(decisions),
               list(team_box),list(players_box), list(skaters), list(goalies), list(onIce),
               list(onIcePlus), list(penaltyBox), list(scratches), list(team_coaches))
      names(game) <- c("all_plays", "scoring_plays", "penalty_plays", "plays_by_period",
                       "current_play", "linescore", "decisions",
                       "team_box", "player_box", "skaters", "goalies", "on_ice", "on_ice_plus",
                       "penalty_box", "scratches", "team_coaches")
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no game feed data for {game_id} available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(game)
}


#' @title **NHL Game PBP**
#' @description Returns information on game pbp for a given game id
#' @param game_id Game unique ID
#' @return Returns a named list of data frames: all_plays
#' @keywords NHL Game PBP
#' @import rvest
#' @importFrom rlang .data
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr mutate filter select rename bind_cols bind_rows
#' @importFrom tidyr unnest unnest_wider everything
#' @importFrom janitor clean_names
#' @export
#' @examples
#' \donttest{
#'    try(nhl_game_pbp(game_id=2021020182))
#' }
nhl_game_pbp <- function(game_id){

  base_url <- "https://statsapi.web.nhl.com/api/v1/game/"

  full_url <- paste0(base_url,
                     game_id,
                     "/feed/live")


  res <- httr::RETRY("GET", full_url)

  # Check the result
  check_status(res)

  tryCatch(
    expr = {
      resp <- res %>%
        httr::content(as = "text", encoding = "UTF-8")
      ###--- Game Rosters ----
      players <- jsonlite::fromJSON(resp)[["gameData"]]
      players_df <- players %>%
        purrr::pluck("players") %>%
        dplyr::tibble() %>%
        tidyr::unnest_wider(".")
      players_df <- players_df %>%
        dplyr::rename(
          "player_id" = "id",
          "player_fullName" = "fullName",
          "player_link" = "link",
          "player_firstName" = "firstName",
          "player_lastName" = "lastName",
          "player_primaryNumber" = "primaryNumber",
          "player_birthDate" = "birthDate",
          "player_currentAge" = "currentAge",
          "player_birthCity" = "birthCity",
          "player_birthStateProvince" = "birthStateProvince",
          "player_birthCountry" = "birthCountry",
          "player_nationality" = "nationality",
          "player_height" = "height",
          "player_weight" = "weight"
        )

      players_df <- players_df %>%
        tidyr::unnest_wider("currentTeam", names_sep = "_") %>%
        tidyr::unnest_wider("primaryPosition", names_sep = "_")
      rosters <- players_df %>%
        dplyr::mutate(
          priority = ifelse(.data$primaryPosition_abbreviation == "G", 2, 1)) %>%
        dplyr::arrange(.data$priority) %>%
        dplyr::select(
          "player_id" = "player_id",
          "player_name" = "player_fullName",
          "position_type" = "primaryPosition_type",
          "position" = "primaryPosition_abbreviation")

      ###--- Game Info ----

      gameInfo <- jsonlite::fromJSON(resp)[["gameData"]]
      game <- gameInfo$game %>%
        dplyr::bind_rows() %>%
        dplyr::rename(
          "game_id" = "pk",
          "season_type" = "type"
        )

      datetime <- gameInfo$datetime %>%
        dplyr::bind_rows()

      # older seasons don't include end time of game

      if("endDateTime" %in% names(datetime)){
        datetime <- datetime %>%
          dplyr::mutate(
            game_start = .data$dateTime %>%
              lubridate::parse_date_time("ymd_HMS") %>%
              lubridate::with_tz("US/Eastern"),
            game_end = .data$endDateTime %>%
              lubridate::parse_date_time("ymd_HMS") %>%
              lubridate::with_tz("US/Eastern"),
            game_date = lubridate::date(.data$game_start),
            game_length = lubridate::as.period(.data$game_end - .data$game_start)
          ) %>%
          dplyr::select("game_date", "game_start", "game_end", "game_length")
      } else {
        datetime <- datetime %>%
          dplyr::mutate(
            game_start = .data$dateTime %>%
              lubridate::parse_date_time("ymd_HMS") %>%
              lubridate::with_tz("US/Eastern"),
            game_date = lubridate::date(.data$game_start),
          ) %>%
          dplyr::select("game_date", "game_start")
      }

      status <- gameInfo[["status"]] %>%
        dplyr::bind_rows() %>%
        dplyr::select("game_state" = "abstractGameState", "detailed_state" = "detailedState")

      venue <- gameInfo[["venue"]] %>%
        dplyr::bind_rows()

      colnames(venue) <- glue::glue("venue_{names(venue)}")

      team_names_keep <- c(
        "home_name","home_abbreviation","home_division_name","home_conference_name","home_id",
        "away_name","away_abbreviation","away_division_name","away_conference_name","away_id"
      )

      teams <- gameInfo$teams %>%
        unlist() %>%
        dplyr::bind_rows() %>%
        janitor::clean_names() %>%
        dplyr::select(dplyr::matches(team_names_keep))

      game_info <- dplyr::bind_cols(
        game, datetime, status, venue, teams
      )
      ###--- Live Data ----

      corsi_events <- c("MISSED_SHOT","SHOT","GOAL","BLOCKED_SHOT")
      fenwick_events <- c("MISSED_SHOT","SHOT","GOAL")
      live_data_df <- jsonlite::fromJSON(resp)[["liveData"]]
      plays_df <- live_data_df$plays$allPlays
      plays_player <- jsonlite::fromJSON(jsonlite::toJSON(plays_df$players), flatten = TRUE)
      # plays_df <- jsonlite::fromJSON(jsonlite::toJSON(plays_df),flatten=TRUE)
      if (length(plays_player) > 0) {
        plays_player_df <- purrr::map_dfr(1:length(plays_player), function(x){
          if("playerType" %in% colnames(plays_player[[x]]) && nrow(plays_player[[x]]) == 4){

            player <- data.frame(
              "event_player_1_type" = ifelse(!is.null(plays_player[[x]]$playerType[[1]]), plays_player[[x]]$playerType[[1]], NA_character_),
              "event_player_1_id" = ifelse(!is.null(plays_player[[x]]$player.id[[1]]),plays_player[[x]]$player.id[[1]], NA_integer_),
              "event_player_1_name" = ifelse(!is.null(plays_player[[x]]$player.fullName[[1]]), plays_player[[x]]$player.fullName[[1]], NA_character_),
              "event_player_1_link" = ifelse(!is.null(plays_player[[x]]$player.link[[1]]), plays_player[[x]]$player.link[[1]], NA_character_),
              "event_player_2_type" = ifelse(!is.null(plays_player[[x]]$playerType[[2]]), plays_player[[x]]$playerType[[2]], NA_character_),
              "event_player_2_id" = ifelse(!is.null(plays_player[[x]]$player.id[[2]]),plays_player[[x]]$player.id[[2]], NA_integer_),
              "event_player_2_name" = ifelse(!is.null(plays_player[[x]]$player.fullName[[2]]), plays_player[[x]]$player.fullName[[2]], NA_character_),
              "event_player_2_link" = ifelse(!is.null(plays_player[[x]]$player.link[[2]]), plays_player[[x]]$player.link[[2]], NA_character_),
              "event_player_3_type" = ifelse(!is.null(plays_player[[x]]$playerType[[3]]), plays_player[[x]]$playerType[[3]], NA_character_),
              "event_player_3_id" = ifelse(!is.null(plays_player[[x]]$player.id[[3]]),plays_player[[x]]$player.id[[3]], NA_integer_),
              "event_player_3_name" = ifelse(!is.null(plays_player[[x]]$player.fullName[[3]]), plays_player[[x]]$player.fullName[[3]], NA_character_),
              "event_player_3_link" = ifelse(!is.null(plays_player[[x]]$player.link[[3]]), plays_player[[x]]$player.link[[3]], NA_character_),
              "event_player_4_type" = ifelse(!is.null(plays_player[[x]]$playerType[[4]]), plays_player[[x]]$playerType[[4]], NA_character_),
              "event_player_4_id" = ifelse(!is.null(plays_player[[x]]$player.id[[4]]),plays_player[[x]]$player.id[[4]], NA_integer_),
              "event_player_4_name" = ifelse(!is.null(plays_player[[x]]$player.fullName[[4]]), plays_player[[x]]$player.fullName[[4]], NA_character_),
              "event_player_4_link" = ifelse(!is.null(plays_player[[x]]$player.link[[4]]), plays_player[[x]]$player.link[[4]], NA_character_))
            return(player)
          }else if("playerType" %in% colnames(plays_player[[x]]) && nrow(plays_player[[x]]) == 3){

            player <- data.frame(
              "event_player_1_type" = ifelse(!is.null(plays_player[[x]]$playerType[[1]]), plays_player[[x]]$playerType[[1]], NA_character_),
              "event_player_1_id" = ifelse(!is.null(plays_player[[x]]$player.id[[1]]),plays_player[[x]]$player.id[[1]], NA_integer_),
              "event_player_1_name" = ifelse(!is.null(plays_player[[x]]$player.fullName[[1]]), plays_player[[x]]$player.fullName[[1]], NA_character_),
              "event_player_1_link" = ifelse(!is.null(plays_player[[x]]$player.link[[1]]), plays_player[[x]]$player.link[[1]], NA_character_),
              "event_player_2_type" = ifelse(!is.null(plays_player[[x]]$playerType[[2]]), plays_player[[x]]$playerType[[2]], NA_character_),
              "event_player_2_id" = ifelse(!is.null(plays_player[[x]]$player.id[[2]]),plays_player[[x]]$player.id[[2]], NA_integer_),
              "event_player_2_name" = ifelse(!is.null(plays_player[[x]]$player.fullName[[2]]), plays_player[[x]]$player.fullName[[2]], NA_character_),
              "event_player_2_link" = ifelse(!is.null(plays_player[[x]]$player.link[[2]]), plays_player[[x]]$player.link[[2]], NA_character_),
              "event_player_3_type" = ifelse(!is.null(plays_player[[x]]$playerType[[3]]), plays_player[[x]]$playerType[[3]], NA_character_),
              "event_player_3_id" = ifelse(!is.null(plays_player[[x]]$player.id[[3]]),plays_player[[x]]$player.id[[3]], NA_integer_),
              "event_player_3_name" = ifelse(!is.null(plays_player[[x]]$player.fullName[[3]]), plays_player[[x]]$player.fullName[[3]], NA_character_),
              "event_player_3_link" = ifelse(!is.null(plays_player[[x]]$player.link[[3]]), plays_player[[x]]$player.link[[3]], NA_character_),
              "event_player_4_type" = NA_character_,
              "event_player_4_id" = NA_integer_,
              "event_player_4_name" = NA_character_,
              "event_player_4_link" = NA_character_)
            return(player)
          }else if("playerType" %in% colnames(plays_player[[x]]) && nrow(plays_player[[x]]) == 2){

            player <- data.frame(
              "event_player_1_type" = ifelse(!is.null(plays_player[[x]]$playerType[[1]]), plays_player[[x]]$playerType[[1]], NA_character_),
              "event_player_1_id" = ifelse(!is.null(plays_player[[x]]$player.id[[1]]),plays_player[[x]]$player.id[[1]], NA_integer_),
              "event_player_1_name" = ifelse(!is.null(plays_player[[x]]$player.fullName[[1]]), plays_player[[x]]$player.fullName[[1]], NA_character_),
              "event_player_1_link" = ifelse(!is.null(plays_player[[x]]$player.link[[1]]), plays_player[[x]]$player.link[[1]], NA_character_),
              "event_player_2_type" = ifelse(!is.null(plays_player[[x]]$playerType[[2]]), plays_player[[x]]$playerType[[2]], NA_character_),
              "event_player_2_id" = ifelse(!is.null(plays_player[[x]]$player.id[[2]]),plays_player[[x]]$player.id[[2]], NA_integer_),
              "event_player_2_name" = ifelse(!is.null(plays_player[[x]]$player.fullName[[2]]), plays_player[[x]]$player.fullName[[2]], NA_character_),
              "event_player_2_link" = ifelse(!is.null(plays_player[[x]]$player.link[[2]]), plays_player[[x]]$player.link[[2]], NA_character_),
              "event_player_3_type" = NA_character_,
              "event_player_3_id" = NA_integer_,
              "event_player_3_name" = NA_character_,
              "event_player_3_link" = NA_character_,
              "event_player_4_type" = NA_character_,
              "event_player_4_id" = NA_integer_,
              "event_player_4_name" = NA_character_,
              "event_player_4_link" = NA_character_)
            return(player)
          }else if("playerType" %in% colnames(plays_player[[x]]) && nrow(plays_player[[x]]) == 1){

            player <- data.frame(
              "event_player_1_type" = ifelse(!is.null(plays_player[[x]]$playerType[[1]]), plays_player[[x]]$playerType[[1]], NA_character_),
              "event_player_1_id" = ifelse(!is.null(plays_player[[x]]$player.id[[1]]),plays_player[[x]]$player.id[[1]], NA_integer_),
              "event_player_1_name" = ifelse(!is.null(plays_player[[x]]$player.fullName[[1]]), plays_player[[x]]$player.fullName[[1]], NA_character_),
              "event_player_1_link" = ifelse(!is.null(plays_player[[x]]$player.link[[1]]), plays_player[[x]]$player.link[[1]], NA_character_),
              "event_player_2_type" = NA_character_,
              "event_player_2_id" = NA_integer_,
              "event_player_2_name" = NA_character_,
              "event_player_2_link" = NA_character_,
              "event_player_3_type" = NA_character_,
              "event_player_3_id" = NA_integer_,
              "event_player_3_name" = NA_character_,
              "event_player_3_link" = NA_character_,
              "event_player_4_type" = NA_character_,
              "event_player_4_id" = NA_integer_,
              "event_player_4_name" = NA_character_,
              "event_player_4_link" = NA_character_)
            return(player)
          }else{
            player <- data.frame(
              "event_player_1_type" = NA_character_,
              "event_player_1_id" = NA_integer_,
              "event_player_1_name" = NA_character_,
              "event_player_1_link" = NA_character_,
              "event_player_2_type" = NA_character_,
              "event_player_2_id" = NA_integer_,
              "event_player_2_name" = NA_character_,
              "event_player_2_link" = NA_character_,
              "event_player_3_type" = NA_character_,
              "event_player_3_id" = NA_integer_,
              "event_player_3_name" = NA_character_,
              "event_player_3_link" = NA_character_,
              "event_player_4_type" = NA_character_,
              "event_player_4_id" = NA_integer_,
              "event_player_4_name" = NA_character_,
              "event_player_4_link" = NA_character_)
            return(player)
          }
        })
      }
      if (length(plays_df) > 0 && length(plays_player) > 0) {
        plays_df <- live_data_df$plays$allPlays
        all_plays <- plays_df %>%
          dplyr::tibble() %>%
          tidyr::unnest_wider("result") %>%
          dplyr::select(-"players")
        if ("strength" %in% names(all_plays)) {
          all_plays <- all_plays %>%
            tidyr::unnest_wider("strength") %>%
            dplyr::rename(
              "strength_code" = "code",
              "strength" = "name")
        }
        all_plays <- all_plays  %>%
          tidyr::unnest_wider("about") %>%
          tidyr::unnest_wider("goals") %>%
          dplyr::rename(
            "home_score" = "home",
            "away_score" = "away")
        if (!is.null(suppressWarnings(all_plays$coordinates))) {
          all_plays <- all_plays %>%
            tidyr::unnest_wider("coordinates")
        }
        all_plays <- all_plays %>%
          tidyr::unnest_wider("team")
        if("triCode" %in% names(all_plays)){
          all_plays <- all_plays %>%
            dplyr::rename(
              "event_team" = "name",
              "event_team_id" = "id",
              "event_team_link" = "link",
              "event_team_abbr" = "triCode"
            )
        } else {
          all_plays <- all_plays %>%
            dplyr::rename(
              "event_team" = "name",
              "event_team_id" = "id",
              "event_team_link" = "link") %>%
            dplyr::mutate(
              event_team_abbr = ifelse(.data$event_team == game_info$home_name,
                                       game_info$home_abbreviation,
                                       game_info$away_abbreviation))
        }

        all_plays <- all_plays %>%
          janitor::clean_names() %>%
          dplyr::mutate(
            # clean up the times
            period_seconds = lubridate::period_to_seconds(lubridate::ms(.data$period_time)),
            game_seconds = .data$period_seconds + (1200 * (.data$period-1)),
            period_seconds_remaining = lubridate::period_to_seconds(lubridate::ms(.data$period_time_remaining)),
            game_seconds_remaining = ifelse(
              .data$period < 4,
              ((3-.data$period) * 1200) + .data$period_seconds_remaining,
              0 - .data$period_seconds),
            home_final = dplyr::last(.data$home_score),
            away_final = dplyr::last(.data$away_score)) %>%
          dplyr::rename("event_type" = "event_type_id")

        # add event players
        players <- plays_player_df
        # combine it all
        pbp <- dplyr::bind_cols(all_plays, players, game_info) %>%
          dplyr::select(1:5,dplyr::all_of(names(players)),dplyr::everything()) %>%
          dplyr::filter(
            # discard redundant rows
            !(.data$event_type %in% c("PERIOD_READY","PERIOD_OFFICIAL","PERIOD_START","GAME_OFFICIAL"))
          )

        # create dummy secondary_type column for preseason/all-star games without one
        if (!("secondary_type" %in% names(pbp))) {
          pbp <- pbp %>%
            dplyr::mutate(secondary_type = NA_character_)
        }

        # create dummy penalty column for preseason/all-star games without one
        if (!("penalty_severity" %in% names(pbp))) {
          pbp <- pbp %>%
            dplyr::mutate(penalty_severity = NA_character_)
        }

        # swap blocked shot event players so
        # shooter is player_1 & blocker is player_2
        if("BLOCKED_SHOT" %in% pbp$event_type){
          pbp_blocks <- pbp %>%
            dplyr::filter(.data$event_type == "BLOCKED_SHOT") %>%
            dplyr::mutate(
              # swap event team to match shooting team instead of blocking team
              event_team = ifelse(.data$event_team == .data$home_name, .data$away_name, .data$home_name),
              event_team_abbr = ifelse(.data$event_team == .data$home_name, .data$home_abbreviation, .data$away_abbreviation),
              event_team_id = ifelse(.data$event_team == .data$home_name, as.integer(.data$home_id), as.integer(.data$away_id)),
              event_team_link = glue::glue("/api/v1/teams/{.data$event_team_id}"),
              blocker_info = glue::glue(
                "{.data$event_player_1_id},{.data$event_player_1_name},{.data$event_player_1_link},{.data$event_player_1_type}"
              ),
              event_player_1_id = .data$event_player_2_id,
              event_player_1_name = .data$event_player_2_name,
              event_player_1_link = .data$event_player_2_link,
              event_player_1_type = .data$event_player_2_type
            ) %>%
            tidyr::separate(
              "blocker_info",
              into = c("event_player_2_id","event_player_2_name",
                       "event_player_2_link","event_player_2_type"),
              sep = ",", remove = TRUE
            ) %>%
            dplyr::mutate(event_player_2_id = as.integer(.data$event_player_2_id))

          pbp <- pbp %>%
            dplyr::filter(.data$event_type != "BLOCKED_SHOT") %>%
            dplyr::bind_rows(pbp_blocks) %>%
            dplyr::arrange(.data$event_idx)
        }
        # replace player_4 with goalie
        # though sometimes there is no player_4
        #   ie if only one goal was scored and it's unassisted,
        #   there will only be two event players

        if("event_player_4_name" %in% names(pbp)){
          pbp <- pbp %>%
            dplyr::mutate(
              event_goalie_id = dplyr::case_when(
                .data$event_player_2_type == "Goalie" ~ .data$event_player_2_id,
                .data$event_player_3_type == "Goalie" ~ .data$event_player_3_id,
                .data$event_player_4_type == "Goalie" ~ .data$event_player_4_id
              ),
              event_goalie_name = dplyr::case_when(
                .data$event_player_2_type == "Goalie" ~ .data$event_player_2_name,
                .data$event_player_3_type == "Goalie" ~ .data$event_player_3_name,
                .data$event_player_4_type == "Goalie" ~ .data$event_player_4_name
              ),
              event_goalie_link = dplyr::case_when(
                .data$event_player_2_type == "Goalie" ~ .data$event_player_2_link,
                .data$event_player_3_type == "Goalie" ~ .data$event_player_3_link,
                .data$event_player_4_type == "Goalie" ~ .data$event_player_4_link
              ),
              event_goalie_type = dplyr::case_when(
                .data$event_player_2_type == "Goalie" ~ "Goalie",
                .data$event_player_3_type == "Goalie" ~ "Goalie",
                .data$event_player_4_type == "Goalie" ~ "Goalie"
              )
            )
        } else if ("event_player_3_name" %in% names(pbp) & !("event_player_4_name" %in% names(pbp))) {
          pbp <- pbp %>%
            dplyr::mutate(
              event_goalie_id = dplyr::case_when(
                .data$event_player_2_type == "Goalie" ~ .data$event_player_2_id,
                .data$event_player_3_type == "Goalie" ~ .data$event_player_3_id
              ),
              event_goalie_name = dplyr::case_when(
                .data$event_player_2_type == "Goalie" ~ .data$event_player_2_name,
                .data$event_player_3_type == "Goalie" ~ .data$event_player_3_name
              ),
              event_goalie_link = dplyr::case_when(
                .data$event_player_2_type == "Goalie" ~ .data$event_player_2_link,
                .data$event_player_3_type == "Goalie" ~ .data$event_player_3_link
              ),
              event_goalie_type = dplyr::case_when(
                .data$event_player_2_type == "Goalie" ~ "Goalie",
                .data$event_player_3_type == "Goalie" ~ "Goalie"
              )
            )
        } else if (!("event_player_3_name" %in% names(pbp))) {
          pbp <- pbp %>%
            dplyr::mutate(
              event_goalie_id = dplyr::case_when(
                .data$event_player_2_type == "Goalie" ~ .data$event_player_2_id
              ),
              event_goalie_name = dplyr::case_when(
                .data$event_player_2_type == "Goalie" ~ .data$event_player_2_name
              ),
              event_goalie_link = dplyr::case_when(
                .data$event_player_2_type == "Goalie" ~ .data$event_player_2_link
              ),
              event_goalie_type = dplyr::case_when(
                .data$event_player_2_type == "Goalie" ~ "Goalie"
              )
            )
        }

        # add shift events

        shifts <- nhl_game_shifts(game_id)

        # if shift data exists
        if(!is.null(shifts)) {
          # MODIFIED FROM THE SCRAPER BY EVOLVING HOCKEY
          # WHO MODIFIED ORIGINAL CODE BY MANNY PERRY

          # combine shift events with pbp events
          pbp_full <- pbp %>%
            dplyr::bind_rows(shifts) %>%
            dplyr::mutate(
              # arrange all events so shift changes are in the proper spot
              priority =
                1 * (.data$event_type %in% c("TAKEAWAY", "GIVEAWAY", "MISSED_SHOT", "HIT", "SHOT", "BLOCKED_SHOT") & !(.data$period == 5 & .data$season_type == "R")) +
                2 * (.data$event_type == "GOAL" & !(.data$period == 5 & .data$season_type == "R")) +
                3 * (.data$event_type == "STOP" & !(.data$period == 5 & .data$season_type == "R")) +
                4 * (.data$event_type == "PENALTY" & !(.data$period == 5 & .data$season_type == "R")) +
                5 * (.data$event_type == "CHANGE" & !(.data$period == 5 & .data$season_type == "R")) +
                6 * (.data$event_type == "PERIOD_END" & !(.data$period == 5 & .data$season_type == "R")) +
                7 * (.data$event_type == "GAME_END" & !(.data$period == 5 & .data$season_type == "R")) +
                8 * (.data$event_type == "FACEOFF" &  !(.data$period == 5 & .data$season_type == "R"))
            ) %>%
            dplyr::arrange(.data$period, .data$game_seconds, .data$priority) %>%
            dplyr::mutate(
              home_index = as.numeric(cumsum(.data$event_type == "CHANGE" &
                                               .data$event_team == unique(pbp$home_name))),
              away_index = as.numeric(cumsum(.data$event_type == "CHANGE" &
                                               .data$event_team == unique(pbp$away_name))),
            ) %>%
            dplyr::select(-"priority")

          # construct a matrix of player names as columns & event row as rows
          home_skaters <- NULL

          for(i in 1:nrow(rosters)){

            player <- rosters$player_name[i]

            skaters_i <- dplyr::tibble(
              on_ice = cumsum(
                1 * stringr::str_detect(
                  dplyr::filter(pbp_full,
                                .data$event_type == "CHANGE" &
                                  .data$event_team == unique(pbp$home_name))$players_on,
                  player) -
                  1 * stringr::str_detect(
                    dplyr::filter(pbp_full,
                                  .data$event_type == "CHANGE" &
                                    .data$event_team == unique(pbp$home_name))$players_off,
                    player)
              )
            )

            suppressMessages({
              home_skaters <- dplyr::bind_cols(home_skaters, skaters_i)
            })

            rm(skaters_i, player)
          }

          colnames(home_skaters) <- rosters$player_name

          home_skaters <- data.frame(home_skaters)

          # transform into matrix of only players on the ice
          on_home <- which(home_skaters == 1, arr.ind = TRUE) %>%
            data.frame() %>%
            dplyr::group_by(.data$row) %>%
            dplyr::summarize(
              home_on_1 = colnames(home_skaters)[unique(.data$col)[1]],
              home_on_2 = colnames(home_skaters)[unique(.data$col)[2]],
              home_on_3 = colnames(home_skaters)[unique(.data$col)[3]],
              home_on_4 = colnames(home_skaters)[unique(.data$col)[4]],
              home_on_5 = colnames(home_skaters)[unique(.data$col)[5]],
              home_on_6 = colnames(home_skaters)[unique(.data$col)[6]],
              home_on_7 = colnames(home_skaters)[unique(.data$col)[7]]
            )

          away_skaters <- NULL

          for(i in 1:nrow(rosters)){

            player <- rosters$player_name[i]

            skaters_i <- dplyr::tibble(
              on_ice = cumsum(
                1 * stringr::str_detect(
                  dplyr::filter(pbp_full,
                                .data$event_type == "CHANGE" &
                                  .data$event_team == unique(pbp$away_name))$players_on,
                  player) -
                  1 * stringr::str_detect(
                    dplyr::filter(pbp_full,
                                  .data$event_type == "CHANGE" &
                                    .data$event_team == unique(pbp$away_name))$players_off,
                    player)
              )
            )

            suppressMessages({
              away_skaters <- dplyr::bind_cols(away_skaters, skaters_i)
            })

            rm(skaters_i, player)
          }

          colnames(away_skaters) <- rosters$player_name

          away_skaters <- data.frame(away_skaters)

          # transform into matrix of only players on the ice
          on_away <- which(away_skaters == 1, arr.ind = TRUE) %>%
            data.frame() %>%
            dplyr::group_by(.data$row) %>%
            dplyr::summarize(
              away_on_1 = colnames(away_skaters)[unique(.data$col)[1]],
              away_on_2 = colnames(away_skaters)[unique(.data$col)[2]],
              away_on_3 = colnames(away_skaters)[unique(.data$col)[3]],
              away_on_4 = colnames(away_skaters)[unique(.data$col)[4]],
              away_on_5 = colnames(away_skaters)[unique(.data$col)[5]],
              away_on_6 = colnames(away_skaters)[unique(.data$col)[6]],
              away_on_7 = colnames(away_skaters)[unique(.data$col)[7]]
            )

          # define goalies
          goalies <- rosters %>%
            dplyr::filter(.data$position == "G") %>%
            dplyr::mutate(
              player_name = stringr::str_replace(.data$player_name, " ", ".")
            ) %>%
            dplyr::mutate(
              player_name = stringr::str_replace(.data$player_name, "-", ".")
            ) %>%
            dplyr::pull("player_name")

          non_plays <- c("GAME_SCHEDULED","PERIOD_END","GAME_END")

          pbp_full <- pbp_full %>%
            dplyr::left_join(on_home, by = c("home_index" = "row")) %>%
            dplyr::left_join(on_away, by = c("away_index" = "row")) %>%
            # adding game info to shift change events and moving info to the end
            dplyr::select(!dplyr::all_of(names(game_info))) %>%
            dplyr::bind_cols(game_info) %>%
            dplyr::mutate(
              # create goalie on-ice columns
              home_goalie = dplyr::case_when(
                .data$home_on_1 %in% goalies ~ .data$home_on_1,
                .data$home_on_2 %in% goalies ~ .data$home_on_2,
                .data$home_on_3 %in% goalies ~ .data$home_on_3,
                .data$home_on_4 %in% goalies ~ .data$home_on_4,
                .data$home_on_5 %in% goalies ~ .data$home_on_5,
                .data$home_on_6 %in% goalies ~ .data$home_on_6,
                .data$home_on_7 %in% goalies ~ .data$home_on_7
              ),
              away_goalie = dplyr::case_when(
                .data$away_on_1 %in% goalies ~ .data$away_on_1,
                .data$away_on_2 %in% goalies ~ .data$away_on_2,
                .data$away_on_3 %in% goalies ~ .data$away_on_3,
                .data$away_on_4 %in% goalies ~ .data$away_on_4,
                .data$away_on_5 %in% goalies ~ .data$away_on_5,
                .data$away_on_6 %in% goalies ~ .data$away_on_6,
                .data$away_on_7 %in% goalies ~ .data$away_on_7
              ),
              # include only skaters in on-ice columns
              # rosters are ordered such that goalies should always be
              # last on-ice skater
              home_on_1 = ifelse(.data$home_on_1 == .data$home_goalie & !is.na(.data$home_goalie), NA_character_, .data$home_on_1),
              home_on_2 = ifelse(.data$home_on_2 == .data$home_goalie & !is.na(.data$home_goalie), NA_character_, .data$home_on_2),
              home_on_3 = ifelse(.data$home_on_3 == .data$home_goalie & !is.na(.data$home_goalie), NA_character_, .data$home_on_3),
              home_on_4 = ifelse(.data$home_on_4 == .data$home_goalie & !is.na(.data$home_goalie), NA_character_, .data$home_on_4),
              home_on_5 = ifelse(.data$home_on_5 == .data$home_goalie & !is.na(.data$home_goalie), NA_character_, .data$home_on_5),
              home_on_6 = ifelse(.data$home_on_6 == .data$home_goalie & !is.na(.data$home_goalie), NA_character_, .data$home_on_6),
              home_on_7 = ifelse(.data$home_on_7 == .data$home_goalie & !is.na(.data$home_goalie), NA_character_, .data$home_on_7),
              away_on_1 = ifelse(.data$away_on_1 == .data$away_goalie & !is.na(.data$away_goalie), NA_character_, .data$away_on_1),
              away_on_2 = ifelse(.data$away_on_2 == .data$away_goalie & !is.na(.data$away_goalie), NA_character_, .data$away_on_2),
              away_on_3 = ifelse(.data$away_on_3 == .data$away_goalie & !is.na(.data$away_goalie), NA_character_, .data$away_on_3),
              away_on_4 = ifelse(.data$away_on_4 == .data$away_goalie & !is.na(.data$away_goalie), NA_character_, .data$away_on_4),
              away_on_5 = ifelse(.data$away_on_5 == .data$away_goalie & !is.na(.data$away_goalie), NA_character_, .data$away_on_5),
              away_on_6 = ifelse(.data$away_on_6 == .data$away_goalie & !is.na(.data$away_goalie), NA_character_, .data$away_on_6),
              away_on_7 = ifelse(.data$away_on_7 == .data$away_goalie & !is.na(.data$away_goalie), NA_character_, .data$away_on_7),
              # create strength states
              home_skaters =
                1 * (!is.na(.data$home_on_1)) + 1 * (!is.na(.data$home_on_2)) +
                1 * (!is.na(.data$home_on_3)) + 1 * (!is.na(.data$home_on_4)) +
                1 * (!is.na(.data$home_on_5)) + 1 * (!is.na(.data$home_on_6)) +
                1 * (!is.na(.data$home_on_7)),
              away_skaters =
                1 * (!is.na(.data$away_on_1)) + 1 * (!is.na(.data$away_on_2)) +
                1 * (!is.na(.data$away_on_3)) + 1 * (!is.na(.data$away_on_4)) +
                1 * (!is.na(.data$away_on_5)) + 1 * (!is.na(.data$away_on_6)) +
                1 * (!is.na(.data$away_on_7)),
              strength_state = dplyr::case_when(
                .data$event_team == .data$home_name ~ glue::glue("{.data$home_skaters}v{.data$away_skaters}"),
                .data$event_team == .data$away_name ~ glue::glue("{.data$away_skaters}v{.data$home_skaters}"),
                TRUE ~ glue::glue("{.data$home_skaters}v{.data$away_skaters}")
              ),
              strength_code = dplyr::case_when(
                .data$home_skaters == .data$away_skaters ~ "EV",
                (.data$home_skaters < .data$away_skaters & .data$event_team == .data$home_name) |
                  (.data$away_skaters < .data$home_skaters & .data$event_team == .data$away_name) ~ "SH",
                (.data$home_skaters < .data$away_skaters & .data$event_team == .data$away_name) |
                  (.data$away_skaters < .data$home_skaters & .data$event_team == .data$home_name) ~ "PP"
              ),
              # fixing the change events at start and end of periods
              strength_code = ifelse(
                .data$event_type %in% non_plays |
                  dplyr::lead(.data$event_type) %in% non_plays |
                  dplyr::lead(dplyr::lead(.data$event_type)) %in% non_plays |
                  dplyr::lag(.data$event_type) %in% non_plays |
                  dplyr::lag(dplyr::lag(.data$event_type)) %in% non_plays,
                NA_character_, .data$strength_code
              ),
              strength = dplyr::case_when(
                .data$strength_code == "EV" ~ "Even",
                .data$strength_code == "SH" ~ "Shorthanded",
                .data$strength_code == "PP" ~ "Power Play"
              ),
              extra_attacker = ifelse(
                ((.data$event_team == .data$home_name & is.na(.data$home_goalie)) |
                   (.data$event_team == .data$away_name & is.na(.data$away_goalie))) &
                  !(.data$event_type %in% non_plays) & .data$event_type != "CHANGE", TRUE, FALSE
              )
            )

          pbp_full <- pbp_full %>%
            dplyr::mutate(
              event_idx = dplyr::row_number()-1,
              home_final = dplyr::last(.data$home_score),
              away_final = dplyr::last(.data$away_score),
              period_seconds_remaining = dplyr::case_when(
                (.data$season_type != "P" & .data$period < 4) | .data$season_type == "P" ~ 1200 - .data$period_seconds,
                .data$season_type != "P" & .data$period == 4 ~ 300 - .data$period_seconds,
                TRUE ~ .data$period_seconds_remaining
              ),
              description = ifelse(
                .data$event_type == "CHANGE",
                glue::glue("ON: {.data$players_on}; OFF: {.data$players_off}"),
                .data$description
              ),
              secondary_type = ifelse(
                .data$event_type == "CHANGE",
                "On the fly",
                .data$secondary_type
              ),
              secondary_type = ifelse(
                .data$event_type == "CHANGE" &
                  (dplyr::lead(.data$event_type) == "FACEOFF" |
                     dplyr::lag(.data$event_type) == "STOP" |
                     dplyr::lag(.data$event_type) == "GOAL" |
                     dplyr::lead(.data$event_type) == "PERIOD_END" |
                     dplyr::lag(.data$event_type) == "PERIOD_END" |
                     dplyr::lead(dplyr::lead(.data$event_type)) == "PERIOD_END" |
                     dplyr::lag(dplyr::lag(.data$event_type)) == "PERIOD_END"),
                "Line change",
                .data$secondary_type
              )
            ) %>%
            # remove unnecessary columns
            dplyr::select(-"event_id",-"home_index",-"away_index",-"event_code")

          # add fixed x & y coordinates so home team shoots right, away shoots left
          pbp_full <- pbp_full %>%
            dplyr::group_by(.data$event_team, .data$period, .data$game_id) %>%
            # find median x shot coordinate to tell us which side teams are shooting on
            dplyr::mutate(med_x = stats::median(.data$x[.data$event_type %in% fenwick_events], na.rm = TRUE)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(
              x_fixed = dplyr::case_when(
                .data$event_team == .data$home_name & .data$med_x > 0 ~ .data$x,
                .data$event_team == .data$home_name & .data$med_x < 0 ~ 0 - .data$x,
                .data$event_team == .data$away_name & .data$med_x > 0 ~ 0 - .data$x,
                .data$event_team == .data$away_name & .data$med_x < 0 ~ .data$x
              ),
              y_fixed = dplyr::case_when(
                .data$event_team == .data$home_name & .data$med_x > 0 ~ .data$y,
                .data$event_team == .data$home_name & .data$med_x < 0 ~ 0 - .data$y,
                .data$event_team == .data$away_name & .data$med_x > 0 ~ 0 - .data$y,
                .data$event_team == .data$away_name & .data$med_x < 0 ~ .data$y
              ),
              # add shot distance/angle
              shot_distance = dplyr::case_when(
                .data$event_team == .data$home_name & .data$event_type %in% fenwick_events ~
                  round(abs(sqrt((.data$x_fixed - 89)^2 + (.data$y_fixed)^2)),1),
                .data$event_team == .data$away_name & .data$event_type %in% fenwick_events ~
                  round(abs(sqrt((.data$x_fixed - (-89))^2 + (.data$y_fixed)^2)),1)
              ),
              shot_angle = dplyr::case_when(
                .data$event_team == .data$home_name & .data$event_type %in% fenwick_events ~
                  round(abs(atan((0-.data$y_fixed) / (89-.data$x_fixed)) * (180 / pi)),1),
                .data$event_team == .data$away_name & .data$event_type %in% fenwick_events ~
                  round(abs(atan((0-.data$y_fixed) / (-89-.data$x_fixed)) * (180 / pi)),1)
              ),
              # fix behind the net angles
              shot_angle = ifelse(
                (.data$event_team == .data$home_name & .data$x_fixed > 89) |
                  (.data$event_team == .data$away_name & .data$x_fixed < -89),
                180 - .data$shot_angle,
                .data$shot_angle
              ),
              event_team_type =  dplyr::case_when(
                .data$event_team == .data$home_name ~ "home",
                .data$event_team == .data$away_name ~ "away"
              )
            ) %>%
            dplyr::select(-"med_x")

          # reorder the columns
          if("event_player_3_name" %in% names(pbp_full)){
            pbp_full <- pbp_full %>%
              # change event player names to match on-ice player name conventions
              dplyr::mutate_at(c("event_player_1_name","event_player_2_name",
                                 "event_player_3_name","event_goalie_name"),
                               ~stringr::str_replace_all(.x, c(" " = ".", "-" = "."))) %>%
              dplyr::select(
                "event_type",
                "event",
                "secondary_type",
                "event_team",
                "event_team_type",
                "description",
                "period",
                "period_seconds",
                "period_seconds_remaining",
                "game_seconds",
                "game_seconds_remaining",
                "home_score",
                "away_score",
                "event_player_1_name",
                "event_player_1_type",
                "event_player_2_name",
                "event_player_2_type",
                "event_player_3_name",
                "event_player_3_type",
                "event_goalie_name",
                "strength_state",
                "strength_code":"event_idx",
                "num_on",
                "players_on",
                "num_off",
                "players_off",
                "extra_attacker",
                "x",
                "y",
                "x_fixed",
                "y_fixed",
                "shot_distance",
                "shot_angle",
                "home_skaters",
                "away_skaters",
                "home_on_1":"away_on_7",
                "home_goalie",
                "away_goalie",
                "game_id",
                "event_idx",
                dplyr::everything()
              )
          } else {
            pbp_full <- pbp_full %>%
              dplyr::mutate_at(c("event_player_1_name","event_player_2_name",
                                 "event_goalie_name"),
                               ~stringr::str_replace_all(.x, c(" " = ".", "-" = "."))) %>%
              dplyr::select(
                "event_type",
                "event",
                "secondary_type",
                "event_team",
                "event_team_type",
                "description",
                "period",
                "period_seconds",
                "period_seconds_remaining",
                "game_seconds",
                "game_seconds_remaining",
                "home_score",
                "away_score",
                "event_player_1_name",
                "event_player_1_type",
                "event_player_2_name",
                "event_player_2_type",
                "event_goalie_name",
                "strength_state",
                "strength_code":"event_idx",
                "num_on",
                "players_on",
                "num_off",
                "players_off",
                "extra_attacker",
                "x",
                "y",
                "x_fixed",
                "y_fixed",
                "shot_distance",
                "shot_angle",
                "home_skaters",
                "away_skaters",
                "home_on_1":"away_on_7",
                "home_goalie",
                "away_goalie",
                "game_id",
                "event_idx",
                dplyr::everything()
              )
          }


          # fill in current score for shift events
          #   first assign score of 0 to start of game to fix issue with
          #   game ID 2018020965, which is missing the "GAME_SCHEDULED" event
          pbp_full$home_score[1] <- 0
          pbp_full$away_score[1] <- 0
          pbp_full <- pbp_full %>%
            tidyr::fill("home_score", .direction = "up") %>%
            tidyr::fill("away_score", .direction = "up")

          # fix period type for shift events
          pbp_full <- pbp_full %>%
            dplyr::mutate(
              period_type = dplyr::case_when(
                .data$period < 4 ~ "REGULAR",
                .data$season_type %in% c("R","PR") & .data$period == 4 ~ "OVERTIME",
                .data$season_type %in% c("R","PR") & .data$period == 5 ~ "SHOOTOUT",
                .data$season_type == "P" & .data$period > 3 ~ "OVERTIME"
              ),
              ordinal_num = dplyr::case_when(
                .data$period == 1 ~ glue::glue("{.data$period}st"),
                .data$period == 2 ~ glue::glue("{.data$period}nd"),
                .data$period == 3 ~ glue::glue("{.data$period}rd"),
                TRUE ~ glue::glue("{.data$period}th")
              )
            )

        } else if(is.null(shifts) & "x" %in% names(pbp)){

          # no shift data available but shot location available
          # ie preseason

          pbp_full <- pbp %>%
            dplyr::select(-"event_id",-"event_code") %>%
            # add fixed x & y coordinates so home team shoots right, away shoots left
            dplyr::group_by(.data$event_team, .data$period, .data$game_id) %>%
            # find median x shot coordinate to tell us which side teams are shooting on
            dplyr::mutate(med_x = stats::median(.data$x[.data$event_type %in% fenwick_events], na.rm = TRUE)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(
              x_fixed = dplyr::case_when(
                .data$event_team == .data$home_name & .data$med_x > 0 ~ .data$x,
                .data$event_team == .data$home_name & .data$med_x < 0 ~ 0 - .data$x,
                .data$event_team == .data$away_name & .data$med_x > 0 ~ 0 - .data$x,
                .data$event_team == .data$away_name & .data$med_x < 0 ~ .data$x
              ),
              y_fixed = dplyr::case_when(
                "event_team" == "home_name" & "med_x" > 0 ~ "y",
                "event_team" == "home_name" & "med_x" < 0 ~ 0 - "y",
                "event_team" == "away_name" & "med_x" > 0 ~ 0 - "y",
                "event_team" == "away_name" & "med_x" < 0 ~ "y"
              ),
              # add shot distance/angle
              shot_distance = dplyr::case_when(
                .data$event_team == .data$home_name & .data$event_type %in% fenwick_events ~
                  round(abs(sqrt((.data$x_fixed - 89)^2 + (.data$y_fixed)^2)),1),
                .data$event_team == .data$away_name & .data$event_type %in% fenwick_events ~
                  round(abs(sqrt((.data$x_fixed - (-89))^2 + (.data$y_fixed)^2)),1)
              ),
              shot_angle = dplyr::case_when(
                .data$event_team == .data$home_name & .data$event_type %in% fenwick_events ~
                  round(abs(atan((0-.data$y_fixed) / (89-.data$x_fixed)) * (180 / pi)),1),
                .data$event_team == .data$away_name & .data$event_type %in% fenwick_events ~
                  round(abs(atan((0-.data$y_fixed) / (-89-.data$x_fixed)) * (180 / pi)),1)
              ),
              # fix behind the net angles
              shot_angle = ifelse(
                (.data$event_team == .data$home_name & .data$x_fixed > 89) |
                  (.data$event_team == .data$away_name & .data$x_fixed < -89),
                180 - .data$shot_angle,
                .data$shot_angle
              ),
              event_team_type =  dplyr::case_when(
                .data$event_team == .data$home_name ~ "home",
                .data$event_team == .data$away_name ~ "away"
              )
            ) %>%
            dplyr::select(-"med_x")

          if("event_player_3_name" %in% names(pbp_full)) {
            pbp_full <- pbp_full %>%
              dplyr::mutate_at(
                c("event_player_1_name","event_player_2_name",
                  "event_player_3_name","event_goalie_name"),
                ~stringr::str_replace_all(.x, c(" " = ".", "-" = "."))
              ) %>%
              dplyr::select(
                "event_type",
                "event",
                "secondary_type",
                "event_team",
                "event_team_type",
                "description",
                "period",
                "period_seconds",
                "period_seconds_remaining",
                "game_seconds",
                "game_seconds_remaining",
                "home_score",
                "away_score",
                "event_player_1_name",
                "event_player_1_type",
                "event_player_2_name",
                "event_player_2_type",
                "event_player_3_name",
                "event_player_3_type",
                "event_goalie_name",
                "penalty_severity":"strength",
                "x",
                "y",
                "x_fixed",
                "y_fixed",
                "shot_distance",
                "shot_angle",
                "game_id",
                "event_idx",
                dplyr::everything()
              )
          } else {
            pbp_full <- pbp_full %>%
              dplyr::mutate_at(
                c("event_player_1_name","event_player_2_name",
                  "event_goalie_name"),
                ~stringr::str_replace_all(.x, c(" " = ".", "-" = "."))
              ) %>%
              dplyr::select(
                "event_type",
                "event",
                "secondary_type",
                "event_team",
                "event_team_type",
                "description",
                "period",
                "period_seconds",
                "period_seconds_remaining",
                "game_seconds",
                "game_seconds_remaining",
                "home_score",
                "away_score",
                "event_player_1_name",
                "event_player_1_type",
                "event_player_2_name",
                "event_player_2_type",
                "event_goalie_name",
                "penalty_severity":"strength",
                "x",
                "y",
                "x_fixed",
                "y_fixed",
                "shot_distance",
                "shot_angle",
                "game_id",
                "event_idx",
                dplyr::everything()
              )
          }

        } else {
          # no shift data or shot location
          if("event_player_3_name" %in% names(pbp)){
            pbp_full <- pbp %>%
              dplyr::mutate_at(
                c("event_player_1_name","event_player_2_name",
                  "event_player_3_name","event_goalie_name"),
                ~stringr::str_replace_all(.x, c(" " = ".", "-" = "."))
              ) %>%
              dplyr::select(-"event_id",-"event_code") %>%
              dplyr::mutate(
                event_team_type =  dplyr::case_when(
                  .data$event_team == .data$home_name ~ "home",
                  .data$event_team == .data$away_name ~ "away"
                )
              ) %>%
              dplyr::select(
                "event_type",
                "event",
                "secondary_type",
                "event_team",
                "event_team_type",
                "description",
                "period",
                "period_time",
                "period_time_remaining",
                "event_player_1_name",
                "event_player_1_type",
                "event_player_2_name",
                "event_player_2_type",
                "event_player_3_name",
                "event_player_3_type",
                "event_goalie_name",
                "game_id",
                "event_idx",
                dplyr::everything()
              )
          } else {
            pbp_full <- pbp %>%
              dplyr::mutate_at(
                c("event_player_1_name","event_player_2_name",
                  "event_goalie_name"),
                ~stringr::str_replace_all(.x, c(" " = ".", "-" = "."))
              ) %>%
              dplyr::select(-"event_id",-"event_code") %>%
              dplyr::mutate(
                event_team_type =  dplyr::case_when(
                  .data$event_team == .data$home_name ~ "home",
                  .data$event_team == .data$away_name ~ "away"
                )
              ) %>%
              dplyr::select(
                "event_type",
                "event",
                "secondary_type",
                "event_team",
                "event_team_type",
                "description",
                "period",
                "period_time",
                "period_time_remaining",
                "event_player_1_name",
                "event_player_1_type",
                "event_player_2_name",
                "event_player_2_type",
                "event_goalie_name",
                "game_id",
                "event_idx",
                dplyr::everything()
              )
          }

        }
        # add event_id
        pbp_full <- pbp_full %>%
          dplyr::mutate(
            event_idx = stringr::str_pad(.data$event_idx, width = 4, side = "left", pad = 0),
            event_id = as.numeric(paste0(.data$game_id,.data$event_idx)),
            secondary_type = ifelse(
              stringr::str_detect(dplyr::lead(.data$description), "PS -") &
                .data$event_type %in% c("SHOT","MISSED_SHOT","GOAL"),
              "Penalty Shot", .data$secondary_type
            )
          )
        pbp_full <- pbp_full %>%
          janitor::clean_names() %>%
          make_fastRhockey_data("NHL Game Plays Information from NHL.com",Sys.time())
      } else {
        pbp_full = data.frame() %>%
          make_fastRhockey_data("NHL Game Plays Information from NHL.com",Sys.time())
      }
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no game plays data for {game_id} available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(pbp_full)
}

