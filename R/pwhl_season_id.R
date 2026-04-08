#' @title  **PWHL Season IDs**
#' @description Retrieves PWHL season IDs from the HockeyTech API.
#'
#' @param season Unused; kept for backwards compatibility.
#' @param game_type Unused; kept for backwards compatibility. Defaults to "regular".
#' @return A data frame with columns:
#'
#'   * `season_id` - Numeric season identifier used by the HockeyTech API.
#'   * `season_name` - Full season name (e.g., "2024-25 Regular Season").
#'   * `season_short` - Short season name.
#'   * `season_yr` - Numeric year derived from the season name (concluding year).
#'   * `game_type_label` - Game type: "preseason", "regular", or "playoffs".
#'   * `career` - Whether this is a career-stats season.
#'   * `playoff` - Whether this is a playoff season.
#'   * `start_date` - Season start date.
#'   * `end_date` - Season end date.
#' @import jsonlite
#' @import dplyr
#' @import httr
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(pwhl_season_id())
#' }

pwhl_season_id <- function(season = NULL, game_type = "regular") {

  tryCatch(
    expr = {
      url <- .pwhl_modulekit_url(list(view = "seasons"))
      r <- .pwhl_api(url)

      seasons_raw <- r$SiteKit$Seasons

      season_df <- data.frame()

      for (i in seq_along(seasons_raw)) {
        s <- seasons_raw[[i]]

        season_row <- data.frame(
          season_id = as.numeric(s$season_id %||% NA),
          season_name = as.character(s$season_name %||% NA),
          season_short = as.character(s$shortname %||% NA),
          career = as.character(s$career %||% "0"),
          playoff = as.character(s$playoff %||% "0"),
          start_date = as.character(s$start_date %||% NA),
          end_date = as.character(s$end_date %||% NA),
          stringsAsFactors = FALSE
        )

        season_df <- dplyr::bind_rows(season_df, season_row)
      }

      # Derive season year and game type from season name
      # Handles both "2024 Regular Season" -> 2024 and "2024-25 Regular Season" -> 2025 (end year)
      season_df <- season_df %>%
        dplyr::mutate(
          season_yr = purrr::map_dbl(.data$season_name, function(nm) {
            # Try "YYYY-YY" format first (e.g., "2024-25 Regular Season")
            m <- stringr::str_match(nm, "(\\d{4})-(\\d{2})")
            if (!is.na(m[1, 1])) {
              start_century <- as.numeric(substr(m[1, 2], 1, 2))
              end_short <- as.numeric(m[1, 3])
              return(start_century * 100 + end_short)
            }
            # Try standalone "YYYY" format (e.g., "2024 Regular Season")
            m2 <- stringr::str_match(nm, "(\\d{4})")
            if (!is.na(m2[1, 1])) {
              return(as.numeric(m2[1, 2]))
            }
            return(NA_real_)
          }),
          game_type_label = dplyr::case_when(
            grepl("[Pp]re[- ]?[Ss]eason", .data$season_name) ~ "preseason",
            grepl("[Pp]layoff|[Pp]ost", .data$season_name) ~ "playoffs",
            TRUE ~ "regular"
          )
        )

      return(season_df)
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Could not retrieve PWHL season data from API. Falling back to hardcoded seasons. Error: {e$message}"))

      # Fallback to hardcoded data
      season_df <- data.frame(
        season_id = c(2, 1, 3, 4, 5, 6, 7, 8),
        season_name = c("2024 Preseason", "2024 Regular Season", "2024 Playoffs",
                        "2024-25 Preseason", "2024-25 Regular Season", "2024-25 Playoffs",
                        "2025-26 Preseason", "2025-26 Regular Season"),
        season_short = c("2024 Pre", "2024 Reg", "2024 Playoffs",
                         "2024-25 Pre", "2024-25 Reg", "2024-25 Playoffs",
                         "2025-26 Pre", "2025-26 Reg"),
        season_yr = c(2024, 2024, 2024, 2025, 2025, 2025, 2026, 2026),
        game_type_label = c("preseason", "regular", "playoffs",
                            "preseason", "regular", "playoffs",
                            "preseason", "regular"),
        career = c("0", "0", "0", "0", "0", "0", "0", "0"),
        playoff = c("0", "0", "1", "0", "0", "1", "0", "0"),
        start_date = rep(NA_character_, 8),
        end_date = rep(NA_character_, 8),
        stringsAsFactors = FALSE
      )

      return(season_df)
    }
  )
}

#' @keywords internal
#' Resolve a season year and game type to a HockeyTech season_id.
#'
#' @param season Numeric year (e.g., 2025)
#' @param game_type Character: "regular", "preseason", or "playoffs"
#' @return Numeric season_id
#' @noRd
.pwhl_resolve_season_id <- function(season, game_type = "regular") {
  seasons <- pwhl_season_id()
  match <- seasons %>%
    dplyr::filter(.data$season_yr == season, .data$game_type_label == game_type)

  if (nrow(match) == 0) {
    stop(glue::glue("No PWHL season found for season={season}, game_type={game_type}"),
         call. = FALSE)
  }

  return(match$season_id[1])
}
