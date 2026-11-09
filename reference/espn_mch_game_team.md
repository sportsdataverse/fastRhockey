# **Get ESPN MCH / WCH Game Team (Competitor Detail, core-v2)**

**Get ESPN MCH / WCH Game Team (Competitor Detail, core-v2)**

## Usage

``` r
espn_mch_game_team(event_id, cid = event_id, team_id, ...)

espn_wch_game_team(event_id, cid = event_id, team_id, ...)
```

## Arguments

- event_id:

  ESPN event (game) identifier.

- cid:

  Competition identifier. Defaults to `event_id`.

- team_id:

  ESPN team/competitor identifier. Use
  [`espn_mch_game_teams()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_game_teams.md)
  to discover valid team ids for a game.

- ...:

  Reserved for forward compatibility.

## Value

A one-row `fastRhockey_data` tibble with competitor detail:

|  |  |  |
|----|----|----|
| col_name | types | description |
| event_id | character | ESPN event id (echoed from arg). |
| cid | character | Competition id (echoed from arg). |
| team_id | character | ESPN team id (echoed from arg). |
| id | character | ESPN competitor identifier. |
| uid | character | Competitor uid string. |
| type | character | Competitor type (e.g. "team"). |
| order | integer | Competitor display order. |
| home_away | character | `"home"` or `"away"`. |
| winner | logical | Whether this competitor won the game. |
| team_ref | character | `$ref` URL for the team-in-season object. |
| score_ref | character | `$ref` URL for the score object. |
| linescores_ref | character | `$ref` URL for the per-period linescores. |
| roster_ref | character | `$ref` URL for the game roster. |
| statistics_ref | character | `$ref` URL for the team game statistics. |
| leaders_ref | character | `$ref` URL for the team game leaders. |
| record_ref | character | `$ref` URL for the team record at the time of the game. |
| competitor_ref | character | `$ref` URL for this competitor object. |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_game_team()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_team.md)
(columns inherited); a collection may be empty.

`espn_wch_game_team()` is the same wrapper for women's college hockey
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
    tms <- espn_mch_game_teams(event_id = eid)
    espn_mch_game_team(event_id = eid, team_id = tms$team_id[1])
  })
#> ── MENS-COLLEGE-HOCKEY Game Team data from ESPN core-v2 ─── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 19:33:46 UTC
#> # A tibble: 1 × 17
#>   event_id  cid       team_id id    uid    type  order home_away winner team_ref
#>   <chr>     <chr>     <chr>   <chr> <chr>  <chr> <int> <chr>     <lgl>  <chr>   
#> 1 401715296 401715296 194     194   s:70~… team      0 home      TRUE   http://…
#> # ℹ 7 more variables: score_ref <chr>, linescores_ref <chr>, roster_ref <chr>,
#> #   statistics_ref <chr>, leaders_ref <chr>, record_ref <chr>,
#> #   competitor_ref <chr>
# }
# \donttest{
  try({
    sb  <- espn_wch_scoreboard(dates = "20250110")
    eid <- sb$game_id[1]
    tms <- espn_wch_game_teams(event_id = eid)
    espn_wch_game_team(event_id = eid, team_id = tms$team_id[1])
  })
#> ✖ 2026-11-09 19:33:47.010601: 
#> ✖ Args: league = "womens-college-hockey", event_id = NULL, cid = NULL
#> ✖ Error: The API returned an error
#> ✖ 2026-11-09 19:33:47.030673: 
#> ✖ Args: league = "womens-college-hockey", event_id = NULL, cid = NULL, team_id = NULL
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
```
