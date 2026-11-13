# **Get ESPN NHL Game Win Probabilities (core-v2)**

**Get ESPN NHL Game Win Probabilities (core-v2)**

## Usage

``` r
espn_nhl_game_probabilities(event_id, cid = event_id, limit = 300, ...)
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
**Note:** This endpoint is sparse for NHL - most games return an empty
tibble with a warning. When populated, columns include:

|  |  |  |
|----|----|----|
| col_name | types | description |
| event_id | character | ESPN event id (echoed from arg). |
| cid | character | Competition id (echoed from arg). |
| sequence_number | integer | Play sequence number this probability is tied to. |
| home_win_percentage | numeric | Home team win probability (0-1). |
| away_win_percentage | numeric | Away team win probability (0-1). |
| tie_percentage | numeric | Tie probability (0-1; typically 0 for NHL). |
| home_toss_up_change | numeric | Change in home team win probability. |
| away_toss_up_change | numeric | Change in away team win probability. |
| play_ref | character | `$ref` URL for the associated play object. |

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
    espn_nhl_game_probabilities(event_id = sb$game_id[1])
  })
#> ✖ 2026-11-13 21:26:58.515747: Invalid arguments or no ESPN nhl game probabilities for event 401688263 available!
#> ✖ Args: league = "nhl", event_id = "401688263", cid = "401688263", limit = 300
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
```
