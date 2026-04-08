#' @title  **PWHL Transactions**
#' @description Retrieves player transactions for a PWHL season.
#'
#' @param season Season (YYYY) to pull transactions from. Defaults to `most_recent_pwhl_season()`.
#' @param game_type Game type: "regular" (default), "preseason", or "playoffs".
#' @return A data frame with transaction records, or NULL if unavailable.
#'
#'   * `transaction_id` - Transaction ID.
#'   * `date` - Transaction date.
#'   * `player_name` - Player name.
#'   * `player_id` - Player ID.
#'   * `team` - Team involved.
#'   * `transaction_type` - Type of transaction.
#'   * `description` - Transaction description.
#' @import jsonlite
#' @import dplyr
#' @import httr
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(pwhl_transactions(season = 2025))
#' }

pwhl_transactions <- function(season = most_recent_pwhl_season(), game_type = "regular") {

  tryCatch(
    expr = {
      season_id <- .pwhl_resolve_season_id(season = season, game_type = game_type)

      url <- .pwhl_modulekit_url(list(
        view = "statviewtype",
        type = "transactions",
        season_id = season_id
      ))

      r <- .pwhl_api(url)

      tx_raw <- r$SiteKit$Statviewtype$transactions

      transactions <- data.frame()

      if (!is.null(tx_raw)) {
        for (i in seq_along(tx_raw)) {
          t <- tx_raw[[i]]
          if (!is.null(t)) {
            tx_row <- as.data.frame(
              lapply(t, function(x) if (is.null(x)) NA else as.character(x)),
              stringsAsFactors = FALSE
            )
            transactions <- dplyr::bind_rows(transactions, tx_row)
          }
        }
      }

      if (nrow(transactions) == 0) {
        message(glue::glue("{Sys.time()}: No transactions found for season={season}."))
        return(NULL)
      }

      transactions <- transactions %>%
        janitor::clean_names()

      transactions <- make_fastRhockey_data(
        transactions,
        type = "PWHL Transactions",
        timestamp = Sys.time()
      )

      return(transactions)
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Error retrieving PWHL transactions. {e$message}"))
      return(NULL)
    }
  )
}
