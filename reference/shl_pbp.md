# Play-by-play events with shot coordinates (2023/24 on)

Wraps `GET https://www.shl.se/api/gameday/play-by-play/{game_uuid}` of
the SHL site API (www.shl.se). Unofficial use of a public source;
seasons are END years (2026 = 2025/26) and are mapped to the site's ssgt
uuids through `/statistics-v2/layout-info`.

## Usage

``` r
shl_pbp(game_uuid, return_parsed = TRUE)
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
| game_source_id | character | StatNet game key, date-HOME-AWAY (e.g. 20250913-FHC-LHC); the same game as gameId. |
| game_id | integer | StatNet game number (StatNet key space, e.g. 22003); keys stats-info games and equals sports-v2 game-info extId. Not the game uuid. |
| event_id | integer | StatNet event sequence number; unique within one game only (pair it with gameId or gameUuid). |
| event_uuid | character | Event uuid (platform key space), unique across games. eventId is the StatNet per-game sequence number. |
| round | integer | Schedule round of the game (1 = first game day). |
| game_type | character | StatNet league label: Elitserien (SHL's name until 2013). |
| arena | character | Name of the arena hosting the game. |
| attendance | integer | Spectators at the game. |
| start_date_and_time | character | Scheduled start (local). |
| period | integer | Period (1-3, 4 = overtime, 5 = shootout). |
| time | character | Game clock in the period (MM:SS, counting up from 00:00). Clock/duration as an mm:ss string. |
| game_state | character | Game state (e.g. GameEnded). |
| revision | integer | Event revision counter. |
| type | character | Event type: goal, shot, penalty, goalkeeper (goalie in / out), period (start / end), timeout. |
| real_world_time | character | Wall-clock time of the event (local). |
| updated_time | character | Last update time of the event. |
| home_team_id | character | Team code (e.g. FHC) in gameday and standings payloads; not the team uuid. |
| home_team_name | character | Team display name (e.g. Frölunda HC). |
| home_team_code | character | Team code (FHC, LHC, ...). |
| home_score | integer | Goals scored by the team in the game. |
| away_team_id | character | Team code (e.g. FHC) in gameday and standings payloads; not the team uuid. |
| away_team_name | character | Team display name (e.g. Frölunda HC). |
| away_team_code | character | Team code (FHC, LHC, ...). |
| away_score | integer | Goals scored by the team in the game. |
| event_team_id | character | Team code (e.g. FHC) in gameday and standings payloads; not the team uuid. |
| event_place | character | Side of the game: home or away. |
| event_team_code | character | Team code (FHC, LHC, ...). |
| event_team_name | character | Team display name (e.g. Frölunda HC). |
| player_id | integer | isa (StatNet) player id, integer. Play-by-play sends it as a string, every other route as an integer; it is cast here so the frames join. |
| player_first_name | character | Given (first) name of the player. |
| player_family_name | character | Surname (family name) of the player. |
| player_jersey_today | character | Jersey number in this game. Numeric value sent as a string. |
| is_entering | logical | Goalie event, true when the goalie enters the ice and false when leaving. |
| game_uuid | character | Platform game uuid (bdhvuc5tex); every gameday route takes it. |
| location_x | integer | Shot x coordinate. Units, origin and orientation unverified; range -29..481 over the 118 located events of game bdhvuc5tex (discovery download, 2026-10-12). |
| location_y | integer | Shot y coordinate. Units, origin and orientation unverified; range -140..129 over the 118 located events of game bdhvuc5tex (discovery download, 2026-10-12). |
| goal_section | integer | Goal-mouth section of the shot/goal (-3 lines up with blocked shots; unverified). |
| is_penalty_shot | logical | Whether the shot/goal was a penalty shot. |
| variant_short_name | character | Penalty type (Minor, Major, ...). |
| variant_minor_time | character | Minor-penalty minutes. Numeric value sent as a string. |
| variant_double_minor_time | character | Double-minor minutes in the penalty. Numeric value sent as a string. |
| variant_bench_time | character | Bench minor minutes in the penalty. Numeric value sent as a string. |
| variant_major_time | character | Major-penalty minutes. Numeric value sent as a string. |
| variant_misconduct_time | character | Misconduct minutes. Numeric value sent as a string. |
| variant_g_m_time | character | Game-misconduct minutes in the penalty. Numeric value sent as a string. |
| variant_m_p_time | character | Match-penalty minutes. Numeric value sent as a string. |
| variant_description | character | Text description (penalty, media). |
| offence | character | Penalty offence code (e.g. CROSS, HOOK, TRIP). |
| did_render_in_penalty_shot | logical | Whether the penalty led to a penalty shot. |
| home_goals | integer | Home goals after this event (running score). |
| away_goals | integer | Away goals after this event (running score). |
| is_empty_net_goal | logical | Whether the goal went into an empty net. |
| pop_player_ids | list | isa ids of the scoring side's players on the ice for the goal (list of integers). |
| nep_player_ids | list | isa ids of the conceding side's players on the ice for the goal (list of integers). |
| assist1_player_id | integer | isa id of the first assist. |
| assist1_first_name | character | First assist: given name. |
| assist1_family_name | character | First assist: surname. |
| assist1_jersey_today | character | First assist: jersey number in this game (string). |
| assist2_player_id | integer | isa id of the second assist. |
| assist2_first_name | character | Second assist: given name. |
| assist2_family_name | character | Second assist: surname. |
| assist2_jersey_today | character | Second assist: jersey number in this game (string). |
| goal_status | character | Goal strength (e.g. EQ, PP1, SH1). |
| started | logical | Whether the period has started. |
| started_at | character | Period start time (ISO 8601 UTC). |
| finished | logical | Whether the period is finished. |
| finished_at | character | Period end time (ISO 8601 UTC). |

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
  shl_pbp(game_uuid = "bdhvuc5tex")
#> ── SHL Pbp Information from www.shl.se ─────────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-13 01:59:19 UTC
#> # A tibble: 131 × 67
#>    game_source_id   game_id event_id event_uuid round game_type arena attendance
#>    <chr>              <int>    <int> <chr>      <int> <chr>     <chr>      <int>
#>  1 20250913-FHC-LHC   22003      133 4209fd58-…     1 Elitseri… Scan…      12044
#>  2 20250913-FHC-LHC   22003      132 067896e4-…     1 Elitseri… Scan…      12044
#>  3 20250913-FHC-LHC   22003      131 2281833f-…     1 Elitseri… Scan…      12044
#>  4 20250913-FHC-LHC   22003      130 8d9c78ae-…     1 Elitseri… Scan…      12044
#>  5 20250913-FHC-LHC   22003      129 1d9e0e35-…     1 Elitseri… Scan…      12044
#>  6 20250913-FHC-LHC   22003      128 aa5f72c3-…     1 Elitseri… Scan…      12044
#>  7 20250913-FHC-LHC   22003      127 7a2afb7d-…     1 Elitseri… Scan…      12044
#>  8 20250913-FHC-LHC   22003      126 881dcda8-…     1 Elitseri… Scan…      12044
#>  9 20250913-FHC-LHC   22003      125 84cf31d2-…     1 Elitseri… Scan…      12044
#> 10 20250913-FHC-LHC   22003      124 f1d7d1a9-…     1 Elitseri… Scan…      12044
#> # ℹ 121 more rows
#> # ℹ 59 more variables: start_date_and_time <chr>, period <int>, time <chr>,
#> #   game_state <chr>, revision <int>, type <chr>, real_world_time <chr>,
#> #   updated_time <chr>, home_team_id <chr>, home_team_name <chr>,
#> #   home_team_code <chr>, home_score <int>, away_team_id <chr>,
#> #   away_team_name <chr>, away_team_code <chr>, away_score <int>,
#> #   event_team_id <chr>, event_place <chr>, event_team_code <chr>, …
# }
```
