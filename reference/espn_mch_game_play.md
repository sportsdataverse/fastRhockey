# **Get ESPN MCH / WCH Game Play Detail (core-v2)**

**Get ESPN MCH / WCH Game Play Detail (core-v2)**

## Usage

``` r
espn_mch_game_play(event_id, cid = event_id, play_id, ...)

espn_wch_game_play(event_id, cid = event_id, play_id, ...)
```

## Arguments

- event_id:

  ESPN event (game) identifier. Use `espn_mch_scoreboard()$game_id` to
  find valid event ids.

- cid:

  Competition identifier. Defaults to `event_id` (for MCH the single
  competition always has the same id as its parent event).

- play_id:

  ESPN play identifier. Use `espn_mch_game_plays(event_id)$id` to find
  valid play ids for a game.

- ...:

  Reserved for forward compatibility.

## Value

A one-row `fastRhockey_data` tibble with play detail:

|                   |           |                                              |
|-------------------|-----------|----------------------------------------------|
| col_name          | types     | description                                  |
| event_id          | character | ESPN event id (echoed from arg).             |
| cid               | character | Competition id (echoed from arg).            |
| play_id           | character | ESPN play id (echoed from arg).              |
| id                | character | ESPN play identifier.                        |
| sequence_number   | integer   | Play sequence number within the game.        |
| type_id           | character | Play type id.                                |
| type_text         | character | Play type label (e.g. "Goal", "Shot").       |
| type_abbreviation | character | Play type abbreviation.                      |
| text              | character | Full play description text.                  |
| short_text        | character | Short play description text.                 |
| away_score        | integer   | Away team score after this play.             |
| home_score        | integer   | Home team score after this play.             |
| period_number     | integer   | Period number (1-3 regulation, 4+ OT).       |
| period_display    | character | Period display string (e.g. "1st").          |
| clock_value       | numeric   | Clock value in seconds.                      |
| clock_display     | character | Clock display string (e.g. "19:34").         |
| scoring_play      | logical   | Whether this play resulted in a goal.        |
| score_value       | integer   | Point value of the scoring play.             |
| priority          | logical   | Whether this is a priority play for display. |
| shooting_play     | logical   | Whether this is a shot on goal.              |
| is_penalty        | logical   | Whether this is a penalty play.              |
| wallclock         | character | Wall-clock UTC timestamp of the play.        |
| play_ref          | character | `$ref` URL for this play object.             |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_game_play()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_play.md)
(columns inherited); a collection may be empty.

`espn_wch_game_play()` is the same wrapper for women's college hockey
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
    sb  <- espn_mch_scoreboard(dates = "20250110")
    eid <- sb$game_id[1]
    pls <- espn_mch_game_plays(event_id = eid)
    espn_mch_game_play(event_id = eid, play_id = pls$id[1])
  })
#> ! 2026-11-09 22:50:18.9588: No plays returned for event 401715296.
#> ── MENS-COLLEGE-HOCKEY Game Play data from ESPN core-v2 ─── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 22:50:19 UTC
#> # A tibble: 1 × 23
#>   event_id  cid       play_id id    sequence_number type_id type_text
#>   <chr>     <chr>     <chr>   <chr>           <int> <chr>   <chr>    
#> 1 401715296 401715296 NA      NA                 NA NA      NA       
#> # ℹ 16 more variables: type_abbreviation <chr>, text <chr>, short_text <chr>,
#> #   away_score <int>, home_score <int>, period_number <int>,
#> #   period_display <chr>, clock_value <dbl>, clock_display <chr>,
#> #   scoring_play <lgl>, score_value <int>, priority <lgl>, shooting_play <lgl>,
#> #   is_penalty <lgl>, wallclock <chr>, play_ref <chr>
# }
# \donttest{
  try({
    sb  <- espn_wch_scoreboard(dates = "20250110")
    eid <- sb$game_id[1]
    pls <- espn_wch_game_plays(event_id = eid)
    espn_wch_game_play(event_id = eid, play_id = pls$id[1])
  })
#> ✖ 2026-11-09 22:50:19.083475: 
#> ✖ Args: league = "womens-college-hockey", event_id = NULL, cid = NULL, limit = 1000
#> ✖ Error: The API returned an error
#> ✖ 2026-11-09 22:50:19.118882: 
#> ✖ Args: league = "womens-college-hockey", event_id = NULL, cid = NULL, play_id = NULL
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
```
