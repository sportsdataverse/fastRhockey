# Team stats per period and total (2023/24 on)

Wraps `GET https://www.shl.se/api/gameday/team-stats/{game_uuid}` of the
SHL site API (www.shl.se). Unofficial use of a public source; seasons
are END years (2026 = 2025/26) and are mapped to the site's ssgt uuids
through `/statistics-v2/layout-info`.

## Usage

``` r
shl_team_stats(game_uuid, return_parsed = TRUE)
```

## Arguments

- game_uuid:

  Game uuid (gameInfo\[\].uuid from /sports-v2/game-schedule).
  gameday/\* has data from 2023/24 on; older games answer 200 with an
  empty body.

- return_parsed:

  Parse the payload into a tibble (default `TRUE`); `FALSE` returns the
  raw decoded JSON as a list.

## Value

A `fastRhockey_data` tibble (the raw JSON list when
`return_parsed = FALSE`) with the following columns:

|  |  |  |
|----|----|----|
| col_name | type | description |
| team_side | character | Side of the game: home or away. |
| game_source_id | character | StatNet game key, date-HOME-AWAY (e.g. 20250913-FHC-LHC); the same game as gameId. |
| game_id | integer | StatNet game number (StatNet key space, e.g. 22003); keys stats-info games and equals sports-v2 game-info extId. Not the game uuid. |
| place | character | Side of the game: home or away. |
| score | integer | Goals scored by the team in the game. |
| team_id | character | Team code (e.g. FHC) in gameday and standings payloads; not the team uuid. |
| team_name | character | Name of the team. |
| team_code | character | StatNet team code (SAIK, FHC). |
| game_uuid | character | Platform game uuid (bdhvuc5tex); every gameday route takes it. |
| period | integer | Period: 0 = the whole game, 1-3 the periods, 4 and up overtime. |
| g | numeric | Number of goals scored. |
| pim | numeric | Penalty minutes. |
| fow | numeric | Number of face-offs won. |

## Errors

`fastRhockey_no_data`: the host answered 404, an empty body (gameday
routes before 2023/24), a table with no rows, or the season and game
type are not in layout-info. `fastRhockey_asset_fetch`: any other
non-2xx answer or a dead connection after retries, a non-JSON or
error-envelope 200, a stats-info page that reaches `count` or falls
short of its `totalCount`, or a body for another game, team, athlete or
ssgt than the one asked for. `fastRhockey_bad_request`: a 400 / 422, a
season that is not a year, or an argument outside its choices.

## See also

Other SHL Functions:
[`shl_athlete_career()`](https://fastRhockey.sportsdataverse.org/reference/shl_athlete_career.md),
[`shl_athlete_details()`](https://fastRhockey.sportsdataverse.org/reference/shl_athlete_details.md),
[`shl_athlete_game_log()`](https://fastRhockey.sportsdataverse.org/reference/shl_athlete_game_log.md),
[`shl_athlete_profile()`](https://fastRhockey.sportsdataverse.org/reference/shl_athlete_profile.md),
[`shl_athlete_seasons()`](https://fastRhockey.sportsdataverse.org/reference/shl_athlete_seasons.md),
[`shl_boxscore()`](https://fastRhockey.sportsdataverse.org/reference/shl_boxscore.md),
[`shl_game_header()`](https://fastRhockey.sportsdataverse.org/reference/shl_game_header.md),
[`shl_game_overview()`](https://fastRhockey.sportsdataverse.org/reference/shl_game_overview.md),
[`shl_game_summary()`](https://fastRhockey.sportsdataverse.org/reference/shl_game_summary.md),
[`shl_last_game_day()`](https://fastRhockey.sportsdataverse.org/reference/shl_last_game_day.md),
[`shl_layout_info()`](https://fastRhockey.sportsdataverse.org/reference/shl_layout_info.md),
[`shl_next_game_day()`](https://fastRhockey.sportsdataverse.org/reference/shl_next_game_day.md),
[`shl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/shl_pbp.md),
[`shl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/shl_player_box.md),
[`shl_player_crosswalk()`](https://fastRhockey.sportsdataverse.org/reference/shl_player_crosswalk.md),
[`shl_player_h2h()`](https://fastRhockey.sportsdataverse.org/reference/shl_player_h2h.md),
[`shl_post_game_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/shl_post_game_team_stats.md),
[`shl_roster()`](https://fastRhockey.sportsdataverse.org/reference/shl_roster.md),
[`shl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/shl_schedule.md),
[`shl_season_filter()`](https://fastRhockey.sportsdataverse.org/reference/shl_season_filter.md),
[`shl_sites()`](https://fastRhockey.sportsdataverse.org/reference/shl_sites.md),
[`shl_ssgt_provider()`](https://fastRhockey.sportsdataverse.org/reference/shl_ssgt_provider.md),
[`shl_ssgt_uuid()`](https://fastRhockey.sportsdataverse.org/reference/shl_ssgt_uuid.md),
[`shl_standings()`](https://fastRhockey.sportsdataverse.org/reference/shl_standings.md),
[`shl_stats_info()`](https://fastRhockey.sportsdataverse.org/reference/shl_stats_info.md),
[`shl_team()`](https://fastRhockey.sportsdataverse.org/reference/shl_team.md),
[`shl_team_played_games()`](https://fastRhockey.sportsdataverse.org/reference/shl_team_played_games.md),
[`shl_team_upcoming_games()`](https://fastRhockey.sportsdataverse.org/reference/shl_team_upcoming_games.md)

## Examples

``` r
# \donttest{
  shl_team_stats(game_uuid = "bdhvuc5tex")
#> ── SHL Team Stats Information from www.shl.se ──────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 06:18:21 UTC
#> # A tibble: 8 × 30
#>   team_side game_source_id   game_id place score team_id team_name     team_code
#>   <chr>     <chr>              <int> <chr> <int> <chr>   <chr>         <chr>    
#> 1 home      20250913-FHC-LHC   22003 home      0 FHC     Frölunda HC   FHC      
#> 2 home      20250913-FHC-LHC   22003 home      0 FHC     Frölunda HC   FHC      
#> 3 home      20250913-FHC-LHC   22003 home      0 FHC     Frölunda HC   FHC      
#> 4 home      20250913-FHC-LHC   22003 home      0 FHC     Frölunda HC   FHC      
#> 5 away      20250913-FHC-LHC   22003 away      0 LHC     Linköpings HC LHC      
#> 6 away      20250913-FHC-LHC   22003 away      0 LHC     Linköpings HC LHC      
#> 7 away      20250913-FHC-LHC   22003 away      0 LHC     Linköpings HC LHC      
#> 8 away      20250913-FHC-LHC   22003 away      0 LHC     Linköpings HC LHC      
#> # ℹ 22 more variables: game_uuid <chr>, period <int>, g <dbl>, pim <dbl>,
#> #   fow <dbl>, sog <dbl>, spg <dbl>, ppsog <dbl>, saves <dbl>, ga <dbl>,
#> #   saves_per_shot <dbl>, pp_perc <dbl>, sh_perc <dbl>, ppg <dbl>, shga <dbl>,
#> #   shg <dbl>, ppga <dbl>, num_pp <dbl>, num_sh <dbl>, hits <dbl>, bk_s <dbl>,
#> #   si_bk <dbl>
# }
```
