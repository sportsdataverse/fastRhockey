# **Get ESPN NHL Game Odds (core-v2)**

**Get ESPN NHL Game Odds (core-v2)**

## Usage

``` r
espn_nhl_game_odds(event_id, cid = event_id, ...)
```

## Arguments

- event_id:

  ESPN event (game) identifier.

- cid:

  Competition identifier. Defaults to `event_id`.

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per odds provider (typically 2
rows per game - consensus + one sportsbook):

|  |  |  |
|----|----|----|
| col_name | types | description |
| event_id | character | ESPN event id (echoed from arg). |
| cid | character | Competition id (echoed from arg). |
| provider_id | character | Odds provider identifier. |
| provider_name | character | Odds provider name. |
| provider_priority | integer | Provider display priority. |
| details | character | Odds detail string (e.g. "DET -185"). |
| over_under | numeric | Over/under total line. |
| spread | numeric | Point spread. |
| over_odds | numeric | Over moneyline odds (American format). |
| under_odds | numeric | Under moneyline odds (American format). |
| moneyline_winner | logical | Whether the moneyline winner was determined. |
| spread_winner | logical | Whether the spread winner was determined. |
| away_team_favorite | logical | Whether the away team was the favorite. |
| away_team_moneyline | integer | Away team moneyline (American format). |
| home_team_favorite | logical | Whether the home team was the favorite. |
| home_team_moneyline | integer | Home team moneyline (American format). |
| open_over | numeric | Opening over decimal odds value. |
| open_under | numeric | Opening under decimal odds value. |
| open_total_american | character | Opening total line in American format (e.g. "5.5"). |
| odds_ref | character | `$ref` URL for this odds object. |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try({
    sb <- espn_nhl_scoreboard(dates = "20250110")
    espn_nhl_game_odds(event_id = sb$game_id[1])
  })
#> ── NHL Game Odds data from ESPN core-v2 ─────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 21:26:54 UTC
#> # A tibble: 2 × 20
#>   event_id  cid   provider_id provider_name provider_priority details over_under
#>   <chr>     <chr> <chr>       <chr>                     <int> <chr>        <dbl>
#> 1 401688263 4016… 58          ESPN BET                      0 DET -1…        5.5
#> 2 401688263 4016… 59          ESPN Bet - L…                 0 DET -5…        7.5
#> # ℹ 13 more variables: spread <dbl>, over_odds <dbl>, under_odds <dbl>,
#> #   moneyline_winner <lgl>, spread_winner <lgl>, away_team_favorite <lgl>,
#> #   away_team_moneyline <int>, home_team_favorite <lgl>,
#> #   home_team_moneyline <int>, open_over <dbl>, open_under <dbl>,
#> #   open_total_american <chr>, odds_ref <chr>
# }
```
