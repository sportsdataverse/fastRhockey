# **Get ESPN NHL Game Plays (core-v2 Play-by-Play)**

**Get ESPN NHL Game Plays (core-v2 Play-by-Play)**

## Usage

``` r
espn_nhl_game_plays(event_id, cid = event_id, limit = 300, ...)
```

## Arguments

- event_id:

  ESPN event (game) identifier. Use `espn_nhl_scoreboard()$game_id` to
  find valid event ids.

- cid:

  Competition identifier. Defaults to `event_id` (for NHL the single
  competition always has the same id as its parent event).

- limit:

  Maximum number of plays to return. Defaults to `300`. A typical NHL
  game has 200-260 plays.

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per play:

|  |  |  |
|----|----|----|
| col_name | types | description |
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
    espn_nhl_game_plays(event_id = sb$game_id[1])
  })
#> ── NHL Game Plays data from ESPN core-v2 ────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 19:34:30 UTC
#> # A tibble: 244 × 27
#>    event_id  cid       id    sequence_number type_id type_text type_abbreviation
#>    <chr>     <chr>     <chr>           <int> <chr>   <chr>     <chr>            
#>  1 401688263 401688263 4016…               2 518     Period S… period-start     
#>  2 401688263 401688263 4016…               3 502     Face Off  faceoff          
#>  3 401688263 401688263 4016…               4 503     Hit       hit              
#>  4 401688263 401688263 4016…               5 506     Shot      shot-on-goal     
#>  5 401688263 401688263 4016…               6 506     Shot      shot-on-goal     
#>  6 401688263 401688263 4016…               7 1402    Giveaway  giveaway         
#>  7 401688263 401688263 4016…               8 507     Missed    shot-missed      
#>  8 401688263 401688263 4016…               9 503     Hit       hit              
#>  9 401688263 401688263 4016…              10 1402    Giveaway  giveaway         
#> 10 401688263 401688263 4016…              11 508     Blocked   shot-blocked     
#> # ℹ 234 more rows
#> # ℹ 20 more variables: text <chr>, short_text <chr>, period_number <int>,
#> #   period_display <chr>, clock_value <dbl>, clock_display <chr>,
#> #   away_score <int>, home_score <int>, scoring_play <lgl>, score_value <int>,
#> #   priority <lgl>, shooting_play <lgl>, is_penalty <lgl>, wallclock <chr>,
#> #   coordinate_x <dbl>, coordinate_y <dbl>, strength_id <chr>,
#> #   strength_text <chr>, team_ref <chr>, play_ref <chr>
# }
```
