# **Get ESPN MCH / WCH Game Plays (core-v2 Play-by-Play)**

**Get ESPN MCH / WCH Game Plays (core-v2 Play-by-Play)**

## Usage

``` r
espn_mch_game_plays(event_id, cid = event_id, limit = 1000, ...)

espn_wch_game_plays(event_id, cid = event_id, limit = 1000, ...)
```

## Arguments

- event_id:

  ESPN event (game) identifier. Use `espn_mch_scoreboard()$game_id` to
  find valid event ids.

- cid:

  Competition identifier. Defaults to `event_id` (for MCH the single
  competition always has the same id as its parent event).

- limit:

  Maximum number of plays to return. Defaults to `1000`. A typical MCH
  game has 200-260 plays.

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per play:

|  |  |  |
|----|----|----|
| col_name | type | description |
| event_id | character | ESPN event id (echoed from arg). |
| cid | character | Competition id (echoed from arg). |
| id | character | ESPN play identifier. |
| sequence_number | integer | Play sequence number within the game. |
| type_id | character | Play type id. |
| type_text | character | Play type label (e.g. "Goal", "Shot", "Face Off"). |
| type_abbreviation | character | Play type abbreviation. |
| text | character | Full play description text. |
| short_text | character | Short play description text. |
| period_number | integer | Period number (1-3 regulation, 4+ OT). |
| period_display | character | Period display string (e.g. "1st"). |
| clock_value | numeric | Clock value in seconds. |
| clock_display | character | Clock display string (e.g. "19:34"). |
| away_score | integer | Away team score after this play. |
| home_score | integer | Home team score after this play. |
| scoring_play | logical | Whether this play resulted in a goal. |
| score_value | integer | Point value of the scoring play (1 for a goal). |
| priority | logical | Whether this is a priority play for display. |
| shooting_play | logical | Whether this is a shot on goal. |
| is_penalty | logical | Whether this is a penalty play. |
| wallclock | character | Wall-clock UTC timestamp of the play. |
| coordinate_x | numeric | X coordinate on the ice surface. |
| coordinate_y | numeric | Y coordinate on the ice surface. |
| strength_id | character | Strength situation id (e.g. even strength, PP, SH). |
| strength_text | character | Strength situation description. |
| team_ref | character | `$ref` URL for the team associated with the play. |
| play_ref | character | `$ref` URL for this play object. |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_game_plays()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_plays.md);
columns verified against a captured MCH body.

`espn_wch_game_plays()` is the same wrapper for women's college hockey
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
    espn_mch_game_plays(event_id = sb$game_id[1])
  })
#> ! 2026-11-13 21:26:09.786241: No plays returned for event 401715296.
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  try({
    sb <- espn_wch_scoreboard(dates = "20250110")
    espn_wch_game_plays(event_id = sb$game_id[1])
  })
#> ✖ 2026-11-13 21:26:09.835669: 
#> ✖ Args: league = "womens-college-hockey", event_id = NULL, cid = NULL, limit = 1000
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
```
