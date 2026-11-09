# Game header: StatNet extId, arena, state, teams, scores

Wraps `GET https://www.shl.se/api/sports-v2/game-info/{game_uuid}` of
the SHL site API (www.shl.se). Unofficial use of a public source;
seasons are END years (2026 = 2025/26) and are mapped to the site's ssgt
uuids through `/statistics-v2/layout-info`.

## Usage

``` r
shl_game_header(game_uuid, return_parsed = TRUE)
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
| instance_context_site_team_is_in_game | logical | Whether the site's team plays in the game. |
| instance_context_site_team_is_home_team | logical | Whether the site's team is the home team. |
| instance_context_site_team_is_away_team | logical | Whether the site's team is the away team. |
| game_info_game_uuid | character | Game uuid (platform key space, e.g. bdhvuc5tex); joins to schedule gameInfo\[\].uuid and every `gameday/{game_uuid}` route. The StatNet number of the same game is gameId. |
| game_info_ext_id | integer | StatNet game number as a string (StatNet key space); equals gameday gameId. Numeric value sent as a string. |
| game_info_start_date_time | character | Scheduled start, local time (Europe/Stockholm). |
| game_info_arena_name | character | Name of the arena hosting the game. |
| game_info_state | character | Game state with an underscore (post_game seen); the schedule spells it post-game. |
| game_info_overtime | logical | Whether the game went to overtime. |
| game_info_shootout | logical | Whether the game went to a shootout. |
| game_info_series_code | character | Series code of the game (SHL). |
| game_info_series_name | character | Series label of the game (SHL). |
| game_info_round_number | integer | Schedule round (1 = first game day of the season). |
| game_info_round_label | character | Round label (often empty). Empty string in every sample (placeholder). |
| game_info_series_display_name | character | Series label shown on the site (SHL). |
| home_team_names_code | character | Display abbreviation; differs from the team code for some clubs (SKE vs SAIK, ÖRE vs OHK). |
| home_team_names_short | character | Short club name ('Herr' = men's team). |
| home_team_names_long | character | Long club name ('Herr' = men's team). |
| home_team_names_full | character | Full club name ('Herr' = men's team). |
| home_team_names_code_site | character | Display abbreviation on the site; 'Herr' = men's team (e.g. 'DIF Herr'). |
| home_team_names_short_site | character | Short club name as shown on the site ('Herr' = men's team). |
| home_team_names_full_site | character | Full club name as shown on the site ('Herr' = men's team). |
| home_team_names_long_site | character | Long club name as shown on the site ('Herr' = men's team). |
| home_team_uuid | character | Team uuid (platform key space, e.g. 087a-087aTQv9u); joins to `/sports-v2/teams/{team_uuid}` and schedule homeTeamInfo/awayTeamInfo.uuid. Not the team code. |
| home_team_instance_id | character | Club site instance id (e.g. fhc1_fhc). |
| home_team_founded_on | character | Club founding date. Numeric value sent as a string. |
| home_team_address | character | Club postal address. |
| home_team_email | character | Contact email address of the club. |
| home_team_icon | character | URL of the team logo image. |
| home_team_score | integer | Goals scored by the team in the game. |
| away_team_names_code | character | Display abbreviation; differs from the team code for some clubs (SKE vs SAIK, ÖRE vs OHK). |
| away_team_names_short | character | Short club name ('Herr' = men's team). |
| away_team_names_long | character | Long club name ('Herr' = men's team). |
| away_team_names_full | character | Full club name ('Herr' = men's team). |
| away_team_names_code_site | character | Display abbreviation on the site; 'Herr' = men's team (e.g. 'DIF Herr'). |
| away_team_names_short_site | character | Short club name as shown on the site ('Herr' = men's team). |
| away_team_names_full_site | character | Full club name as shown on the site ('Herr' = men's team). |
| away_team_names_long_site | character | Long club name as shown on the site ('Herr' = men's team). |
| away_team_uuid | character | Team uuid (platform key space, e.g. 087a-087aTQv9u); joins to `/sports-v2/teams/{team_uuid}` and schedule homeTeamInfo/awayTeamInfo.uuid. Not the team code. |
| away_team_instance_id | character | Club site instance id (e.g. fhc1_fhc). |
| away_team_founded_on | character | Club founding date. Numeric value sent as a string. |
| away_team_address | character | Club postal address. |
| away_team_email | character | Contact email address of the club. |
| away_team_icon | character | URL of the team logo image. |
| away_team_score | integer | Goals scored by the team in the game. |
| ssgt_uuid | character | ssgt uuid (platform key space: one season + series + game type); label via layout-info originalSeasonSeriesGameTypeTeams\[\]. |
| series_uuid | character | Series uuid (platform key space; SHL = qQ9-bb0bzEWUk); the seriesUuid param of game-schedule. |

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

[family_shl](https://fastRhockey.sportsdataverse.org/reference/family_shl.md)
lists every function in this family.

## Examples

``` r
# \donttest{
  try(shl_game_header(game_uuid = "bdhvuc5tex"))
#> ── SHL Game Header Information from www.shl.se ──────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 11:53:54 UTC
#> # A tibble: 1 × 47
#>   instance_context_site_team_is_…¹ instance_context_sit…² instance_context_sit…³
#>   <lgl>                            <lgl>                  <lgl>                 
#> 1 TRUE                             TRUE                   TRUE                  
#> # ℹ abbreviated names: ¹​instance_context_site_team_is_in_game,
#> #   ²​instance_context_site_team_is_home_team,
#> #   ³​instance_context_site_team_is_away_team
#> # ℹ 44 more variables: game_info_game_uuid <chr>, game_info_ext_id <int>,
#> #   game_info_start_date_time <chr>, game_info_arena_name <chr>,
#> #   game_info_state <chr>, game_info_overtime <lgl>, game_info_shootout <lgl>,
#> #   game_info_series_code <chr>, game_info_series_name <chr>, …
# }
```
