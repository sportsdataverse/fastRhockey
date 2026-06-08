# **PWHL Transactions**

Retrieves player transactions for a PWHL season.

## Usage

``` r
pwhl_transactions(season = most_recent_pwhl_season(), game_type = "regular")
```

## Arguments

- season:

  Season (YYYY) to pull transactions from. Defaults to
  [`most_recent_pwhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_pwhl_season.md).

- game_type:

  Game type: "regular" (default), "preseason", or "playoffs".

## Value

A data frame (`fastRhockey_data`) with the following columns:

|                            |           |                                  |
|----------------------------|-----------|----------------------------------|
| col_name                   | types     | description                      |
| transaction_type           | character | Type of transaction.             |
| title                      | character | Transaction title/headline.      |
| ttype                      | character | Transaction type code.           |
| ttype_text                 | character | Transaction type description.    |
| transaction_date           | character | Transaction date.                |
| transaction_time           | character | Transaction time.                |
| formatted_transaction_date | character | Human-readable transaction date. |
| timezone                   | character | Time zone of the transaction.    |
| player_id                  | character | Unique player identifier.        |
| response1                  | character | First response/detail field.     |
| response2                  | character | Second response/detail field.    |
| first_name                 | character | Player first name.               |
| last_name                  | character | Player last name.                |
| player_name                | character | Full player name.                |
| position                   | character | Player position.                 |
| team_id                    | character | Unique team identifier.          |
| team_city                  | character | Team city.                       |
| team_name                  | character | Team name.                       |
| team_code                  | character | Team abbreviation/code.          |
| division                   | character | Division identifier.             |
| team_logo                  | character | URL to the team logo image.      |
| detail                     | character | Additional transaction detail.   |

## Examples

``` r
# \donttest{
  try(pwhl_transactions(season = 2025))
#> ── PWHL Transactions ────────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-06-08 11:45:23 UTC
#> # A tibble: 20 × 22
#>    transaction_type title     ttype ttype_text transaction_date transaction_time
#>    <chr>            <chr>     <chr> <chr>      <chr>            <chr>           
#>  1 2                Signed    1     ADD        2025-03-31       "16:32:23"      
#>  2 2                Signed    1     ADD        2025-03-25       "22:00:45"      
#>  3 4                Traded f… 0     DEL        2025-03-21       "14:15:00"      
#>  4 2                Signed    1     ADD        2025-03-14       "14:35:51"      
#>  5 4                Traded f… 0     DEL        2025-03-13       ""              
#>  6 2                Signed    1     ADD        2025-03-13       "21:44:40"      
#>  7 3                Traded to 1     ADD        2025-03-13       ""              
#>  8 3                Traded to 1     ADD        2025-03-13       ""              
#>  9 4                Traded f… 0     DEL        2025-03-13       ""              
#> 10 2                Signed    1     ADD        2025-03-11       "12:00:00"      
#> 11 2                Signed    1     ADD        2025-02-11       "19:06:40"      
#> 12 4                Traded f… 0     DEL        2025-01-21       ""              
#> 13 3                Traded to 1     ADD        2025-01-21       ""              
#> 14 3                Traded to 1     ADD        2025-01-21       ""              
#> 15 4                Traded f… 0     DEL        2025-01-21       ""              
#> 16 1                Retired   0     DEL        2025-01-04       "21:33:00"      
#> 17 3                Traded to 1     ADD        2024-12-30       ""              
#> 18 4                Traded f… 0     DEL        2024-12-30       ""              
#> 19 3                Traded to 1     ADD        2024-12-30       ""              
#> 20 4                Traded f… 0     DEL        2024-12-30       ""              
#> # ℹ 16 more variables: formatted_transaction_date <chr>, timezone <chr>,
#> #   player_id <chr>, response1 <chr>, response2 <chr>, first_name <chr>,
#> #   last_name <chr>, player_name <chr>, position <chr>, team_id <chr>,
#> #   team_city <chr>, team_name <chr>, team_code <chr>, division <chr>,
#> #   team_logo <chr>, detail <chr>
# }
```
