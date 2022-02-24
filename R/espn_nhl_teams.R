#' Get ESPN NHL team names and ids
#' @author Saiem Gilani
#' @return A teams data frame
#' @keywords NHL Teams
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr filter select rename bind_cols bind_rows row_number group_by mutate as_tibble ungroup
#' @importFrom tidyr unnest unnest_wider everything pivot_wider
#' @import rvest
#' @export
#' @examples
#' \donttest{
#'   try(espn_nhl_teams())
#' }
espn_nhl_teams <- function(){
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old))
  play_base_url <- "http://site.api.espn.com/apis/site/v2/sports/hockey/nhl/teams"

  res <- httr::RETRY("GET", play_base_url)

  # Check the result
  check_status(res)

  resp <- res %>%
    httr::content(as = "text", encoding = "UTF-8")

  tryCatch(
    expr = {

      leagues <- jsonlite::fromJSON(resp)[["sports"]][["leagues"]][[1]][['teams']][[1]][['team']] %>%
        dplyr::group_by(.data$id) %>%
        tidyr::unnest_wider(.data$logos, names_sep = "_") %>%
        tidyr::unnest_wider(.data$logos_href, names_sep = "_") %>%
        dplyr::select(-.data$logos_width,-.data$logos_height,
                      -.data$logos_alt, -.data$logos_rel) %>%
        dplyr::ungroup()
      if("records" %in% colnames(leagues)){
        records <- leagues$record
        records<- records %>%
          tidyr::unnest_wider(.data$items) %>%
          tidyr::unnest_wider(.data$stats, names_sep = "_") %>%
          dplyr::mutate(row = dplyr::row_number())
        stat <- records %>%
          dplyr::group_by(.data$row) %>%
          purrr::map_if(is.data.frame, list)
        stat <- lapply(stat$stats_1,function(x) x %>%
                         purrr::map_if(is.data.frame,list) %>%
                         dplyr::as_tibble())

        s <- lapply(stat, function(x) {
          tidyr::pivot_wider(x)
        })

        s <- tibble::tibble(g = s)
        stats <- s %>% unnest_wider(.data$g)

        records <- dplyr::bind_cols(records %>% dplyr::select(.data$summary), stats)
        leagues <- leagues %>% dplyr::select(
          -.data$record
        )
      }
      leagues <- leagues %>% dplyr::select(
        -.data$links,
        -.data$isActive,
        -.data$isAllStar,
        -.data$uid,
        -.data$slug,
        -.data$record,
        -.data$logos_lastUpdated)
      teams <- leagues %>%
        dplyr::rename(
          logo = .data$logos_href_1,
          logo_dark = .data$logos_href_2,
          mascot = .data$name,
          team = .data$location,
          espn_team_id = .data$id,
          short_name = .data$shortDisplayName,
          alternate_color = .data$alternateColor,
          display_name = .data$displayName
        )
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no teams data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(teams)
}
