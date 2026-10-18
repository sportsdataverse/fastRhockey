# Extended skater and goalie game stats per team (2023/24 on)

Wraps `GET https://www.shl.se/api/gameday/player-stats/{game_uuid}` of
the SHL site API (www.shl.se). Unofficial use of a public source;
seasons are END years (2026 = 2025/26) and are mapped to the site's ssgt
uuids through `/statistics-v2/layout-info`.

## Usage

``` r
shl_player_box(game_uuid, return_parsed = TRUE)
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
| position_group | character | skater or goalie (goalies come from the body's gkStats rows). |
| player_id | integer | isa (StatNet) player id, integer. Play-by-play sends it as a string, every other route as an integer; it is cast here so the frames join. |
| team_id | character | Team code (e.g. FHC) in gameday and standings payloads; not the team uuid. |
| period | integer | Period: 0 = the whole game, 1-3 the periods, 4 and up overtime. |
| full_name | character | Given name and surname. |
| first_name | character | Given (first) name of the player. |
| last_name | character | Surname (family name) of the player. |
| nr | integer | Jersey (shirt) number of the player. |
| pos | character | Position code in this game (e.g. CE, LW, RW, D, GK). |
| line | integer | Line (unit) number the player was listed on. |
| g | numeric | Number of goals scored. |
| a | numeric | Number of assists credited. |
| ppg | numeric | Power-play goals. |
| sw | numeric | Number of shots that missed the net. |
| pim | numeric | Penalty minutes. |
| sog | numeric | Number of shots on goal. |
| ppsog | numeric | Power-play shots on goal. |
| plus_minus | numeric | Plus/minus in the game. |
| toi | character | Time on ice (mm:ss). Clock/duration as an mm:ss string. |
| hits | numeric | Body checks delivered. |
| fow | numeric | Number of face-offs won. |
| fol | numeric | Face-offs lost. |
| fo_perc | numeric | Face-off win %. |
| ga | numeric | Number of goals conceded (goals against). |
| soga | numeric | Shots on goal against. |
| spga | numeric | Unverified goalie column (box score). |
| svs | numeric | Number of saves made by the goalie. |
| svs_perc | numeric | Save percentage of the game as a FRACTION 0-1 (e.g. '0.9655...'); integer 0 for a goalie without shots. Numeric, but sent as a string in some rows and as a number in others. |
| tp | numeric | Points (goals + assists). |
| shg | numeric | Shorthanded goals. |
| nep | numeric | On ice for goals against (count). |
| pop | numeric | On ice for goals for (count). |
| fo_tot | numeric | Face-offs taken. |
| num_pen | numeric | Number of penalties. |
| ms | numeric | Missed shots (wide + blocked). |
| bk_s | numeric | Number of blocked shots. |

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
  shl_player_box(game_uuid = "bdhvuc5tex")
#> ── SHL Player Box Information from www.shl.se ──────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 05:51:26 UTC
#> # A tibble: 44 × 37
#>    team_side position_group player_id team_id period full_name        first_name
#>    <chr>     <chr>              <int> <chr>    <int> <chr>            <chr>     
#>  1 home      skater              4724 NA           0 Christian Folin  Christian 
#>  2 home      skater              3910 NA           0 Filip Hasa       Filip     
#>  3 home      skater              1332 NA           0 Henrik Tömmernes Henrik    
#>  4 home      skater              6711 NA           0 Max Westergård   Max       
#>  5 home      skater              3274 NA           0 Max Lindholm     Max       
#>  6 home      skater              2004 NA           0 Max Friberg      Max       
#>  7 home      skater              5799 NA           0 Isac Born        Isac      
#>  8 home      skater              5639 NA           0 Jere Innala      Jere      
#>  9 home      skater              6433 NA           0 Arttu Ruotsalai… Arttu     
#> 10 home      skater              4092 NA           0 Isac Heens       Isac      
#> # ℹ 34 more rows
#> # ℹ 30 more variables: last_name <chr>, nr <int>, pos <chr>, line <int>,
#> #   g <dbl>, a <dbl>, ppg <dbl>, sw <dbl>, pim <dbl>, sog <dbl>, ppsog <dbl>,
#> #   plus_minus <dbl>, toi <chr>, hits <dbl>, fow <dbl>, fol <dbl>,
#> #   fo_perc <dbl>, ga <dbl>, soga <dbl>, spga <dbl>, svs <dbl>, svs_perc <dbl>,
#> #   tp <dbl>, shg <dbl>, nep <dbl>, pop <dbl>, fo_tot <dbl>, num_pen <dbl>,
#> #   ms <dbl>, bk_s <dbl>
# }
```
