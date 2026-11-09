# **Get ESPN NHL Game Play Detail (core-v2)**

**Get ESPN NHL Game Play Detail (core-v2)**

## Usage

``` r
espn_nhl_game_play(event_id, cid = event_id, play_id, ...)
```

## Arguments

- event_id:

  ESPN event (game) identifier. Use `espn_nhl_scoreboard()$game_id` to
  find valid event ids.

- cid:

  Competition identifier. Defaults to `event_id` (for NHL the single
  competition always has the same id as its parent event).

- play_id:

  ESPN play identifier. Use `espn_nhl_game_plays(event_id)$id` to find
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

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try({
    sb  <- espn_nhl_scoreboard(dates = "20250110")
    eid <- sb$game_id[1]
    pls <- espn_nhl_game_plays(event_id = eid)
    espn_nhl_game_play(event_id = eid, play_id = pls$id[1])
  })
#> ── NHL Game Play data from ESPN core-v2 ─────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 17:19:06 UTC
#> # A tibble: 1 × 23
#>   event_id  cid       play_id            id    sequence_number type_id type_text
#>   <chr>     <chr>     <chr>              <chr>           <int> <chr>   <chr>    
#> 1 401688263 401688263 401688263000000520 4016…               2 518     Period S…
#> # ℹ 16 more variables: type_abbreviation <chr>, text <chr>, short_text <chr>,
#> #   away_score <int>, home_score <int>, period_number <int>,
#> #   period_display <chr>, clock_value <dbl>, clock_display <chr>,
#> #   scoring_play <lgl>, score_value <int>, priority <lgl>, shooting_play <lgl>,
#> #   is_penalty <lgl>, wallclock <chr>, play_ref <chr>
# }
```
