# espn_nhl_teams.R
# Public wrapper + internal league-generic helper for ESPN hockey teams.

# ---------------------------------------------------------------------------
# Internal league-generic helper
# ---------------------------------------------------------------------------

#' Internal: ESPN hockey teams (league-generic)
#'
#' @param league ESPN league slug. Defaults to `"nhl"`.
#' @param ... Passed through to `.retry_request()` (e.g. `proxy =`).
#' @return A `fastRhockey_data` tibble of team metadata.
#' @noRd
.espn_hockey_teams <- function(league = "nhl", ...) {
  .args <- .capture_args()

  teams <- data.frame()

  tryCatch(
    expr = {
      raw <- .espn_hockey_request("site_v2", "teams", league = league, ...)

      leagues <- raw[["sports"]][["leagues"]][[1]][["teams"]][[1]][["team"]] %>%
        dplyr::group_by(.data$id) %>%
        tidyr::unnest_wider("logos", names_sep = "_") %>%
        tidyr::unnest_wider("logos_href", names_sep = "_") %>%
        dplyr::select(
          -dplyr::any_of("logos_width"),
          -dplyr::any_of("logos_height"),
          -dplyr::any_of("logos_alt"),
          -dplyr::any_of("logos_rel")
        ) %>%
        dplyr::ungroup()

      if ("records" %in% colnames(leagues)) {
        records <- leagues$record
        records <- records %>%
          tidyr::unnest_wider("items") %>%
          tidyr::unnest_wider("stats", names_sep = "_") %>%
          dplyr::mutate(row = dplyr::row_number())
        stat <- records %>%
          dplyr::group_by(.data$row) %>%
          purrr::map_if(is.data.frame, list)
        stat <- lapply(stat$stats_1, function(x) x %>%
                         purrr::map_if(is.data.frame, list) %>%
                         dplyr::as_tibble())

        s <- lapply(stat, function(x) {
          tidyr::pivot_wider(x)
        })

        s <- tibble::tibble(g = s)
        stats <- s %>%
          tidyr::unnest_wider("g")

        records <- dplyr::bind_cols(records %>% dplyr::select("summary"), stats)
        leagues <- leagues %>%
          dplyr::select(-dplyr::any_of("record"))
      }

      leagues <- leagues %>%
        dplyr::select(
          -dplyr::any_of("links"),
          -dplyr::any_of("isActive"),
          -dplyr::any_of("isAllStar"),
          -dplyr::any_of("uid"),
          -dplyr::any_of("slug")
        )

      teams <- leagues %>%
        dplyr::rename(
          "logo"            = "logos_href_1",
          "logo_dark"       = "logos_href_2",
          "mascot"          = "name",
          "team"            = "location",
          "espn_team_id"    = "id",
          "short_name"      = "shortDisplayName",
          "alternate_color" = "alternateColor",
          "display_name"    = "displayName"
        ) %>%
        dplyr::mutate(
          espn_team_id = as.integer(.data$espn_team_id)
        )

      teams <- teams %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Teams data from ESPN.com"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} teams data available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} teams",
        args = .args)
    },
    finally = {}
  )
  return(teams)
}

# ---------------------------------------------------------------------------
# Public NHL shim
# ---------------------------------------------------------------------------

#' @title **Get ESPN NHL team names and IDs**
#' @author Saiem Gilani
#' @param ... Additional arguments passed through to the underlying request
#'   (e.g. `proxy =`).
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name        |types     |description                                  |
#'    |:---------------|:---------|:--------------------------------------------|
#'    |abbreviation    |character |Team abbreviation.                           |
#'    |alternate_color |character |Team alternate color hex code.               |
#'    |color           |character |Team primary color hex code.                 |
#'    |display_name    |character |Full team display name.                      |
#'    |espn_team_id    |integer   |ESPN team identifier.                        |
#'    |team            |character |Team location name.                          |
#'    |logo            |character |URL to the team logo.                        |
#'    |logo_dark       |character |URL to the dark-variant team logo.           |
#'    |logos_href_3    |character |URL to an additional team logo variant.      |
#'    |logos_href_4    |character |URL to an additional team logo variant.      |
#'    |logos_href_5    |character |URL to an additional team logo variant.      |
#'    |logos_href_6    |character |URL to an additional team logo variant.      |
#'    |logos_href_7    |character |URL to an additional team logo variant.      |
#'    |logos_href_8    |character |URL to an additional team logo variant.      |
#'    |logos_href_9    |character |URL to an additional team logo variant.      |
#'    |logos_href_10   |character |URL to an additional team logo variant.      |
#'    |logos_href_11   |character |URL to an additional team logo variant.      |
#'    |logos_href_12   |character |URL to an additional team logo variant.      |
#'    |logos_href_13   |character |URL to an additional team logo variant.      |
#'    |logos_href_14   |character |URL to an additional team logo variant.      |
#'    |logos_href_15   |character |URL to an additional team logo variant.      |
#'    |logos_href_16   |character |URL to an additional team logo variant.      |
#'    |mascot          |character |Team mascot name.                            |
#'    |nickname        |character |Team nickname.                               |
#'    |short_name      |character |Short team display name.                     |
#'
#' @keywords NHL Teams
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr filter select rename bind_cols bind_rows row_number group_by mutate as_tibble ungroup any_of
#' @importFrom tidyr unnest unnest_wider everything pivot_wider
#' @importFrom purrr map_if
#' @export
#' @examples
#' \donttest{
#'   try(espn_nhl_teams())
#' }
espn_nhl_teams <- function(...) {
  .espn_hockey_teams(league = "nhl", ...)
}
