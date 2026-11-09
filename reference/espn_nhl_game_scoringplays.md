# **Get ESPN NHL Game Scoring Plays (core-v2)**

**Get ESPN NHL Game Scoring Plays (core-v2)**

## Usage

``` r
espn_nhl_game_scoringplays(event_id, cid = event_id, limit = 300, ...)
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
returns 404 for NHL; this function falls back to fetching all plays via
[`espn_nhl_game_plays()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_plays.md)
and filtering to rows where `scoring_play == TRUE`. Columns are the same
as
[`espn_nhl_game_plays()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_plays.md):

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
| score_value | integer | Always 1 for NHL goals. |
| coordinate_x | numeric | X coordinate of the shot. |
| coordinate_y | numeric | Y coordinate of the shot. |
| strength_text | character | Strength situation (e.g. "Even Strength", "Power Play"). |
| team_ref | character | `$ref` URL for the scoring team. |

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
    espn_nhl_game_scoringplays(event_id = sb$game_id[1])
  })
#> ── NHL Game Plays data from ESPN core-v2 ────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 22:51:09 UTC
#> # A tibble: 8 × 27
#>   event_id cid   id    sequence_number type_id type_text type_abbreviation text 
#>   <chr>    <chr> <chr>           <int> <chr>   <chr>     <chr>             <chr>
#> 1 4016882… 4016… 4016…              15 505     Goal      goal              Alex…
#> 2 4016882… 4016… 4016…              33 505     Goal      goal              Ryan…
#> 3 4016882… 4016… 4016…              77 505     Goal      goal              Luca…
#> 4 4016882… 4016… 4016…             196 505     Goal      goal              Andr…
#> 5 4016882… 4016… 4016…             204 505     Goal      goal              Albe…
#> 6 4016882… 4016… 4016…             221 505     Goal      goal              Teuv…
#> 7 4016882… 4016… 4016…             238 505     Goal      goal              Teuv…
#> 8 4016882… 4016… 4016…             242 505     Goal      goal              Marc…
#> # ℹ 19 more variables: short_text <chr>, period_number <int>,
#> #   period_display <chr>, clock_value <dbl>, clock_display <chr>,
#> #   away_score <int>, home_score <int>, scoring_play <lgl>, score_value <int>,
#> #   priority <lgl>, shooting_play <lgl>, is_penalty <lgl>, wallclock <chr>,
#> #   coordinate_x <dbl>, coordinate_y <dbl>, strength_id <chr>,
#> #   strength_text <chr>, team_ref <chr>, play_ref <chr>
# }
```
