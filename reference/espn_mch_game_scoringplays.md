# **Get ESPN MCH / WCH Game Scoring Plays (core-v2)**

**Get ESPN MCH / WCH Game Scoring Plays (core-v2)**

## Usage

``` r
espn_mch_game_scoringplays(event_id, cid = event_id, limit = 300, ...)

espn_wch_game_scoringplays(event_id, cid = event_id, limit = 300, ...)
```

## Arguments

- event_id:

  ESPN event (game) identifier.

- cid:

  Competition identifier. Defaults to `event_id`.

- limit:

  Maximum number of plays to fetch for filtering. Defaults to `300`.
  Only used when the dedicated `scoringplays` endpoint is unavailable
  and the function falls back to filtering `game_plays()`.

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per scoring play (goal).
**Implementation note:** The dedicated `/scoringplays` core-v2 endpoint
returns 404 for MCH; this function falls back to fetching all plays via
[`espn_mch_game_plays()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_game_plays.md)
and filtering to rows where `scoring_play == TRUE`. Columns are the same
as
[`espn_mch_game_plays()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_game_plays.md):

|  |  |  |
|----|----|----|
| col_name | types | description |
| event_id | character | ESPN event id (echoed from arg). |
| cid | character | Competition id (echoed from arg). |
| id | character | ESPN play identifier. |
| sequence_number | integer | Play sequence number within the game. |
| type_id | character | Play type id. |
| type_text | character | Play type label (always "Goal" for scoring plays). |
| text | character | Full play description text. |
| period_number | integer | Period number of the goal. |
| clock_display | character | Clock time of the goal. |
| away_score | integer | Away team score after the goal. |
| home_score | integer | Home team score after the goal. |
| scoring_play | logical | Always `TRUE`. |
| score_value | integer | Always 1 for MCH goals. |
| coordinate_x | numeric | X coordinate of the shot. |
| coordinate_y | numeric | Y coordinate of the shot. |
| strength_text | character | Strength situation (e.g. "Even Strength", "Power Play"). |
| team_ref | character | `$ref` URL for the scoring team. |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_game_scoringplays()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_scoringplays.md)
(columns inherited); a collection may be empty.

`espn_wch_game_scoringplays()` is the same wrapper for women's college
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
    espn_mch_game_scoringplays(event_id = sb$game_id[1])
  })
#> ! 2026-11-09 11:49:12.989129: No plays returned for event 401715296.
#> ! 2026-11-09 11:49:12.999684: No plays data available to filter for scoring plays (event 401715296).
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  try({
    sb <- espn_wch_scoreboard(dates = "20250110")
    espn_wch_game_scoringplays(event_id = sb$game_id[1])
  })
#> ✖ 2026-11-09 11:49:13.049065: 
#> ✖ Args: league = "womens-college-hockey", event_id = NULL, cid = NULL, limit = 300
#> ✖ Error: The API returned an error
#> ! 2026-11-09 11:49:13.056979: No plays data available to filter for scoring plays (event ).
#> data frame with 0 columns and 0 rows
# }
```
