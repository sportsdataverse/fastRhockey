# **Get ESPN MCH / WCH Game Win Probabilities (core-v2)**

**Get ESPN MCH / WCH Game Win Probabilities (core-v2)**

## Usage

``` r
espn_mch_game_probabilities(event_id, cid = event_id, limit = 300, ...)

espn_wch_game_probabilities(event_id, cid = event_id, limit = 300, ...)
```

## Arguments

- event_id:

  ESPN event (game) identifier.

- cid:

  Competition identifier. Defaults to `event_id`.

- limit:

  Maximum number of probability entries to return. Defaults to `300`.

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per play probability entry.
**Note:** This endpoint is sparse for MCH - most games return an empty
tibble with a warning. When populated, columns include:

|  |  |  |
|----|----|----|
| col_name | types | description |
| event_id | character | ESPN event id (echoed from arg). |
| cid | character | Competition id (echoed from arg). |
| sequence_number | integer | Play sequence number this probability is tied to. |
| home_win_percentage | numeric | Home team win probability (0-1). |
| away_win_percentage | numeric | Away team win probability (0-1). |
| tie_percentage | numeric | Tie probability (0-1; typically 0 for MCH). |
| home_toss_up_change | numeric | Change in home team win probability. |
| away_toss_up_change | numeric | Change in away team win probability. |
| play_ref | character | `$ref` URL for the associated play object. |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_game_probabilities()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_probabilities.md)
(columns inherited); a collection may be empty.

`espn_wch_game_probabilities()` is the same wrapper for women's college
hockey (league slug `womens-college-hockey`): same arguments, same
columns.

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
    espn_mch_game_probabilities(event_id = sb$game_id[1])
  })
#> ✖ 2026-11-13 21:26:11.299562: Invalid arguments or no ESPN mens-college-hockey game probabilities for event 401715296 available!
#> ✖ Args: league = "mens-college-hockey", event_id = "401715296", cid = "401715296", limit = 300
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  try({
    sb <- espn_wch_scoreboard(dates = "20250110")
    espn_wch_game_probabilities(event_id = sb$game_id[1])
  })
#> ✖ 2026-11-13 21:26:11.351418: 
#> ✖ Args: league = "womens-college-hockey", event_id = NULL, cid = NULL, limit = 300
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
```
