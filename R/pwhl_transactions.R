#' @title  **PWHL Transactions**
#' @description Retrieves player transactions for a PWHL season.
#'
#' @param season Season (YYYY) to pull transactions from. Defaults to `most_recent_pwhl_season()`.
#' @param game_type Game type: "regular" (default), "preseason", or "playoffs".
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name                   |types     |description                              |
#'    |:--------------------------|:---------|:----------------------------------------|
#'    |transaction_type           |character |Type of transaction.                     |
#'    |title                      |character |Transaction title/headline.              |
#'    |ttype                      |character |Transaction type code.                   |
#'    |ttype_text                 |character |Transaction type description.            |
#'    |transaction_date           |character |Transaction date.                        |
#'    |transaction_time           |character |Transaction time.                        |
#'    |formatted_transaction_date |character |Human-readable transaction date.         |
#'    |timezone                   |character |Time zone of the transaction.            |
#'    |player_id                  |character |Unique player identifier.                |
#'    |response1                  |character |First response/detail field.             |
#'    |response2                  |character |Second response/detail field.            |
#'    |first_name                 |character |Player first name.                       |
#'    |last_name                  |character |Player last name.                        |
#'    |player_name                |character |Full player name.                        |
#'    |position                   |character |Player position.                         |
#'    |team_id                    |character |Unique team identifier.                  |
#'    |team_city                  |character |Team city.                               |
#'    |team_name                  |character |Team name.                               |
#'    |team_code                  |character |Team abbreviation/code.                  |
#'    |division                   |character |Division identifier.                     |
#'    |team_logo                  |character |URL to the team logo image.              |
#'    |detail                     |character |Additional transaction detail.           |
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
