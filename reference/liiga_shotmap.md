# **Liiga shotmap**

One game's shots with rink-image coordinates (pixels), shooter, blocker
and skaters on ice; from 2016.

## Usage

``` r
liiga_shotmap(season, game_id)
```

## Arguments

- season:

  Season END year (2026 = 2025-26). Game ids repeat across seasons, so a
  game is (season, game_id).

- game_id:

  Game id (game_id in liiga_schedule or liiga_games), e.g. 1; unique
  only within a season.

## Value

A `fastRhockey_data` tibble (15 columns; zero rows when a valid payload
has none).

|  |  |  |
|----|----|----|
| col_name | type | description |
| `season` | integer | Season END year (2026 = 2025-26). |
| `game_id` | integer | Game id, unique only within a season: key a game as (season, game_id). |
| `type` | character | Strength of the shot, e.g. EvenStrengthShot, PowerplayShot. |
| `shooting_team_id` | integer | Team id of the shooter's team (the number of the `<num>:<slug>` key). |
| `left_team_id` | integer | Team whose shots are drawn at the right end of the rink image (x grows toward it). |
| `right_team_id` | integer | Team whose shots are drawn at the left end of the rink image. |
| `game_time` | integer | Seconds from the start of the game. Regular season: 3900 (65:00) = the end of the 5-minute overtime; playoff overtime runs past it. |
| `period` | integer | Period number: 1-3 are regulation, 4 and up overtime (or the shootout). |
| `shot_x` | integer | Shot x on the site's rink drawing. Game 1 of 2026: x 90-932; the leftTeam's shots lie at x 188-932 and the rightTeam's at 90-379, so x grows toward the right end. Origin, unit and image width are not stated (unverified). |
| `shot_y` | integer | Shot y on the site's rink drawing. Game 1 of 2026: y 9-514. Origin, direction and image height are not stated (unverified). |
| `blocker_id` | integer | Player id of the goalkeeper or skater who stopped the shot, else null. |
| `shooter_id` | integer | Shooter's player id. |
| `event_type` | character | Shot outcome: GOAL, GOALIE_BLOCKED (saved), MISSED, PLAYER_BLOCKED. |
| `own_team_players_on_ice` | integer | Shooting team's skaters on ice. |
| `other_team_players_on_ice` | integer | Defending team's skaters on ice. |

## Details

Endpoint: `GET https://liiga.fi/api/v2/shotmap/{season}/{game_id}`

Seasons are END years (2026 = 2025-26), the same key as sdv-py. Requests
to liiga.fi are spaced at least one second apart. A request Liiga
answers with nothing (an unknown game, a tournament not played that
season, a shot map before 2016) raises `fastRhockey_no_data`; a failed
fetch or an error-shaped answer raises `fastRhockey_asset_fetch`; a bad
argument raises `fastRhockey_bad_request`.

## See also

Other Liiga Functions:
[`liiga_game()`](https://fastRhockey.sportsdataverse.org/reference/liiga_game.md),
[`liiga_game_goals()`](https://fastRhockey.sportsdataverse.org/reference/liiga_game_goals.md),
[`liiga_game_lineups()`](https://fastRhockey.sportsdataverse.org/reference/liiga_game_lineups.md),
[`liiga_game_penalties()`](https://fastRhockey.sportsdataverse.org/reference/liiga_game_penalties.md),
[`liiga_game_players_stats()`](https://fastRhockey.sportsdataverse.org/reference/liiga_game_players_stats.md),
[`liiga_game_shootout()`](https://fastRhockey.sportsdataverse.org/reference/liiga_game_shootout.md),
[`liiga_game_stats()`](https://fastRhockey.sportsdataverse.org/reference/liiga_game_stats.md),
[`liiga_game_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/liiga_game_team_stats.md),
[`liiga_games()`](https://fastRhockey.sportsdataverse.org/reference/liiga_games.md),
[`liiga_gameweeks()`](https://fastRhockey.sportsdataverse.org/reference/liiga_gameweeks.md),
[`liiga_goals()`](https://fastRhockey.sportsdataverse.org/reference/liiga_goals.md),
[`liiga_player()`](https://fastRhockey.sportsdataverse.org/reference/liiga_player.md),
[`liiga_player_career()`](https://fastRhockey.sportsdataverse.org/reference/liiga_player_career.md),
[`liiga_player_game_log()`](https://fastRhockey.sportsdataverse.org/reference/liiga_player_game_log.md),
[`liiga_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/liiga_player_stats.md),
[`liiga_players_info()`](https://fastRhockey.sportsdataverse.org/reference/liiga_players_info.md),
[`liiga_schedule()`](https://fastRhockey.sportsdataverse.org/reference/liiga_schedule.md),
[`liiga_standings()`](https://fastRhockey.sportsdataverse.org/reference/liiga_standings.md),
[`liiga_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/liiga_team_stats.md),
[`liiga_teams()`](https://fastRhockey.sportsdataverse.org/reference/liiga_teams.md),
[`liiga_teams_history()`](https://fastRhockey.sportsdataverse.org/reference/liiga_teams_history.md),
[`liiga_tournament()`](https://fastRhockey.sportsdataverse.org/reference/liiga_tournament.md)

## Examples

``` r
# \donttest{
  try(liiga_shotmap(game_id = 1, season = 2026))
#> ── Liiga shotmap Information from liiga.fi ─────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 06:14:01 UTC
#> # A tibble: 90 × 16
#>    season game_id type     shooting_team_id left_team_id right_team_id game_time
#>     <int>   <int> <chr>               <int>        <int>         <int>     <int>
#>  1   2026       1 EvenStr…        651304385    651304385      55786244        13
#>  2   2026       1 EvenStr…        651304385    651304385      55786244        18
#>  3   2026       1 EvenStr…        651304385    651304385      55786244        62
#>  4   2026       1 Powerpl…         55786244    651304385      55786244       102
#>  5   2026       1 Powerpl…         55786244    651304385      55786244       151
#>  6   2026       1 Powerpl…         55786244    651304385      55786244       160
#>  7   2026       1 EvenStr…        651304385    651304385      55786244       223
#>  8   2026       1 EvenStr…        651304385    651304385      55786244       283
#>  9   2026       1 EvenStr…        651304385    651304385      55786244       293
#> 10   2026       1 EvenStr…        651304385    651304385      55786244       320
#> # ℹ 80 more rows
#> # ℹ 9 more variables: period <int>, shot_x <int>, shot_y <int>,
#> #   blocker_id <int>, shooter_id <int>, event_type <chr>,
#> #   own_team_players_on_ice <int>, other_team_players_on_ice <int>, xg <dbl>
# }
```
