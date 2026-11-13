# **Get ESPN NHL Game Team Roster (core-v2)**

**Get ESPN NHL Game Team Roster (core-v2)**

## Usage

``` r
espn_nhl_game_team_roster(event_id, cid = event_id, team_id, ...)
```

## Arguments

- event_id:

  ESPN event (game) identifier.

- cid:

  Competition identifier. Defaults to `event_id`.

- team_id:

  ESPN team/competitor identifier.

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per rostered player (typically
~23 skaters + goalies per team):

|                |           |                                              |
|----------------|-----------|----------------------------------------------|
| col_name       | types     | description                                  |
| event_id       | character | ESPN event id (echoed from arg).             |
| cid            | character | Competition id (echoed from arg).            |
| team_id        | character | ESPN team id (echoed from arg).              |
| player_id      | character | ESPN player/athlete identifier.              |
| jersey         | character | Player jersey number.                        |
| display_name   | character | Player display name.                         |
| scratched      | logical   | Whether the player was a healthy scratch.    |
| scratch_reason | character | Reason for scratch (if applicable).          |
| athlete_id     | character | Athlete id parsed from the `$ref` URL.       |
| athlete_ref    | character | `$ref` URL for the athlete-in-season object. |
| position_id    | character | Position id parsed from the `$ref` URL.      |
| position_ref   | character | `$ref` URL for the player's position.        |
| statistics_ref | character | `$ref` URL for the player's game statistics. |

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
    tms <- espn_nhl_game_teams(event_id = eid)
    espn_nhl_game_team_roster(event_id = eid, team_id = tms$team_id[1])
  })
#> ── NHL Game Team Roster data from ESPN core-v2 ──────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 20:16:35 UTC
#> # A tibble: 23 × 13
#>    event_id cid   team_id player_id jersey display_name scratched scratch_reason
#>    <chr>    <chr> <chr>   <chr>     <chr>  <chr>        <lgl>     <chr>         
#>  1 4016882… 4016… 5       3735      88     Kane         FALSE     NA            
#>  2 4016882… 4016… 5       5246      8      Chiarot      FALSE     NA            
#>  3 4016882… 4016… 5       5734      39     Talbot       FALSE     NA            
#>  4 4016882… 4016… 5       5763      3      Holl         FALSE     NA            
#>  5 4016882… 4016… 5       5837      11     Tarasenko    FALSE     NA            
#>  6 4016882… 4016… 5       3041995   37     Compher      FALSE     NA            
#>  7 4016882… 4016… 5       3042114   18     Copp         FALSE     NA            
#>  8 4016882… 4016… 5       3042138   14     Motte        FALSE     NA            
#>  9 4016882… 4016… 5       3114755   71     Larkin       FALSE     NA            
#> 10 4016882… 4016… 5       3151137   35     Husso        FALSE     NA            
#> # ℹ 13 more rows
#> # ℹ 5 more variables: athlete_id <chr>, athlete_ref <chr>, position_id <chr>,
#> #   position_ref <chr>, statistics_ref <chr>
# }
```
