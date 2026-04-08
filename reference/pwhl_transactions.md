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

A data frame with transaction records, or NULL if unavailable.

- `transaction_id` - Transaction ID.

- `date` - Transaction date.

- `player_name` - Player name.

- `player_id` - Player ID.

- `team` - Team involved.

- `transaction_type` - Type of transaction.

- `description` - Transaction description.

## Examples

``` r
# \donttest{
  try(pwhl_transactions(season = 2025))
#> ── PWHL Transactions ────────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-08 02:42:43 UTC
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
