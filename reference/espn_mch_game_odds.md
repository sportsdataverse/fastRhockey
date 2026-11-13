# **Get ESPN MCH / WCH Game Odds (core-v2)**

**Get ESPN MCH / WCH Game Odds (core-v2)**

## Usage

``` r
espn_mch_game_odds(event_id, cid = event_id, ...)

espn_wch_game_odds(event_id, cid = event_id, ...)
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

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_game_odds()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_odds.md)
(columns inherited); a collection may be empty.

`espn_wch_game_odds()` is the same wrapper for women's college hockey
(league slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try({
    sb <- espn_mch_scoreboard(dates = "20250110")
    espn_mch_game_odds(event_id = sb$game_id[1])
  })
#> ! 2026-11-13 20:15:49.89499: No odds returned for event 401715296.
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  try({
    sb <- espn_wch_scoreboard(dates = "20250110")
    espn_wch_game_odds(event_id = sb$game_id[1])
  })
#> ✖ 2026-11-13 20:15:49.994755: 
#> ✖ Args: league = "womens-college-hockey", event_id = NULL, cid = NULL
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
```
