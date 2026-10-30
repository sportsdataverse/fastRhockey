# Score, state, clock, StatNet gameId and gameSourceId

Wraps `GET https://www.shl.se/api/gameday/game-overview/{game_uuid}` of
the SHL site API (www.shl.se). Unofficial use of a public source;
seasons are END years (2026 = 2025/26) and are mapped to the site's ssgt
uuids through `/statistics-v2/layout-info`.

## Usage

``` r
shl_game_overview(game_uuid, return_parsed = TRUE)
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
| game_uuid | character | Platform game uuid (bdhvuc5tex); every gameday route takes it. |
| home_team_game_source_id | character | StatNet game key, date-HOME-AWAY (e.g. 20250913-FHC-LHC); the same game as gameId. |
| home_team_game_id | integer | StatNet game number (StatNet key space, e.g. 22003); keys stats-info games and equals sports-v2 game-info extId. Not the game uuid. |
| home_team_place | character | Side of the game: home or away. |
| home_team_score | integer | Goals scored by the team in the game. |
| home_team_team_id | character | Team code (e.g. FHC) in gameday and standings payloads; not the team uuid. |
| home_team_team_name | character | Team display name (e.g. Frölunda HC). |
| home_team_team_code | character | Team code (FHC, LHC, ...). |
| home_team_game_uuid | character | Game uuid (platform key space, e.g. bdhvuc5tex); joins to schedule gameInfo\[\].uuid and every `gameday/{game_uuid}` route. The StatNet number of the same game is gameId. |
| away_team_game_source_id | character | StatNet game key, date-HOME-AWAY (e.g. 20250913-FHC-LHC); the same game as gameId. |
| away_team_game_id | integer | StatNet game number (StatNet key space, e.g. 22003); keys stats-info games and equals sports-v2 game-info extId. Not the game uuid. |
| away_team_place | character | Side of the game: home or away. |
| away_team_score | integer | Goals scored by the team in the game. |
| away_team_team_id | character | Team code (e.g. FHC) in gameday and standings payloads; not the team uuid. |
| away_team_team_name | character | Team display name (e.g. Frölunda HC). |
| away_team_team_code | character | Team code (FHC, LHC, ...). |
| away_team_game_uuid | character | Game uuid (platform key space, e.g. bdhvuc5tex); joins to schedule gameInfo\[\].uuid and every `gameday/{game_uuid}` route. The StatNet number of the same game is gameId. |
| home_goals | integer | Home goals after this event (running score). |
| away_goals | integer | Away goals after this event (running score). |
| state | character | StatNet game state (GameEnded seen). |
| time_period | integer | Period (1-3, 4 = overtime, 5 = shootout). |
| time_period_time | character | Time in period (MM:SS). Clock/duration as an mm:ss string. |

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
[`shl_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/shl_team_stats.md),
[`shl_team_upcoming_games()`](https://fastRhockey.sportsdataverse.org/reference/shl_team_upcoming_games.md)

## Examples

``` r
# \donttest{
  shl_game_overview(game_uuid = "bdhvuc5tex")
#> ── SHL Game Overview Information from www.shl.se ───── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-30 00:20:03 UTC
#> # A tibble: 1 × 22
#>   game_uuid  home_team_game_source_id home_team_game_id home_team_place
#>   <chr>      <chr>                                <int> <chr>          
#> 1 bdhvuc5tex 20250913-FHC-LHC                     22003 home           
#> # ℹ 18 more variables: home_team_score <int>, home_team_team_id <chr>,
#> #   home_team_team_name <chr>, home_team_team_code <chr>,
#> #   home_team_game_uuid <chr>, away_team_game_source_id <chr>,
#> #   away_team_game_id <int>, away_team_place <chr>, away_team_score <int>,
#> #   away_team_team_id <chr>, away_team_team_name <chr>,
#> #   away_team_team_code <chr>, away_team_game_uuid <chr>, home_goals <int>,
#> #   away_goals <int>, state <chr>, time_period <int>, time_period_time <chr>
# }
```
