# **Get ESPN MCH / WCH Game Team Roster (core-v2)**

**Get ESPN MCH / WCH Game Team Roster (core-v2)**

## Usage

``` r
espn_mch_game_team_roster(event_id, cid = event_id, team_id, ...)

espn_wch_game_team_roster(event_id, cid = event_id, team_id, ...)
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

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_game_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_team_roster.md)
(columns inherited); a collection may be empty.

`espn_wch_game_team_roster()` is the same wrapper for women's college
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
    sb  <- espn_mch_scoreboard(dates = "20250110")
    eid <- sb$game_id[1]
    tms <- espn_mch_game_teams(event_id = eid)
    espn_mch_game_team_roster(event_id = eid, team_id = tms$team_id[1])
  })
#> ── MENS-COLLEGE-HOCKEY Game Team Roster data from ESPN core-v2 ─────────────────
#> ℹ Data updated: 2026-11-13 21:26:16 UTC
#> # A tibble: 22 × 13
#>    event_id cid   team_id player_id jersey display_name scratched scratch_reason
#>    <chr>    <chr> <chr>   <chr>     <chr>  <chr>        <lgl>     <chr>         
#>  1 4017152… 4017… 194     2334891   0      Bench        FALSE     NA            
#>  2 4017152… 4017… 194     4419697   20     M. Cassidy   FALSE     NA            
#>  3 4017152… 4017… 194     4419706   11     K. Sadlocha  FALSE     NA            
#>  4 4017152… 4017… 194     4419711   29     G. Westlund  FALSE     NA            
#>  5 4017152… 4017… 194     4996076   8      S. Brickey   FALSE     NA            
#>  6 4017152… 4017… 194     5136695   2      J. Marooney  FALSE     NA            
#>  7 4017152… 4017… 194     5136697   4      J. Larkin    FALSE     NA            
#>  8 4017152… 4017… 194     5136699   7      E. McIntyre  FALSE     NA            
#>  9 4017152… 4017… 194     5136701   13     T. Singleton FALSE     NA            
#> 10 4017152… 4017… 194     5136702   14     D. Messina   FALSE     NA            
#> # ℹ 12 more rows
#> # ℹ 5 more variables: athlete_id <chr>, athlete_ref <chr>, position_id <chr>,
#> #   position_ref <chr>, statistics_ref <chr>
# }
# \donttest{
  try({
    sb  <- espn_wch_scoreboard(dates = "20250110")
    eid <- sb$game_id[1]
    tms <- espn_wch_game_teams(event_id = eid)
    espn_wch_game_team_roster(event_id = eid, team_id = tms$team_id[1])
  })
#> ✖ 2026-11-13 21:26:16.542613: 
#> ✖ Args: league = "womens-college-hockey", event_id = NULL, cid = NULL
#> ✖ Error: The API returned an error
#> ✖ 2026-11-13 21:26:16.57808: 
#> ✖ Args: league = "womens-college-hockey", event_id = NULL, cid = NULL, team_id = NULL
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
```
