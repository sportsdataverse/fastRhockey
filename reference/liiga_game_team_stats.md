# **Liiga game team stats**

One game's per-period team stats with puck possession.

## Usage

``` r
liiga_game_team_stats(season, game_id)
```

## Arguments

- season:

  Season END year (2026 = 2025-26). Game ids repeat across seasons, so a
  game is (season, game_id).

- game_id:

  Game id (game_id in liiga_schedule or liiga_games), e.g. 1; unique
  only within a season.

## Value

A `fastRhockey_data` tibble (22 columns; zero rows when a valid payload
has none).

|  |  |  |
|----|----|----|
| col_name | type | description |
| `season` | integer | Season END year (2026 = 2025-26). |
| `game_id` | integer | Game id, unique only within a season: key a game as (season, game_id). |
| `side` | character | Which team of the game the row belongs to: home or away. |
| `period` | integer | Period number: 1-3 are regulation, 4 and up overtime (or the shootout). |
| `team_id` | integer | Team id, the number of Liiga's `<num>:<slug>` team key (sent as that string, a bare integer or a digit string depending on the route). |
| `team_slug` | character | Team slug, the part of Liiga's `<num>:<slug>` team key after the colon (e.g. hpk). |
| `goals` | integer | Goals (team, this period). |
| `shots` | integer | Shots (team, this period). |
| `power_play_instances` | character | JSON list of Liiga's codes for the team's power plays in the period, one per power play (code meaning undocumented). |
| `power_play_goals` | integer | Power play goals (team, this period). |
| `short_handed_instances` | character | JSON list of Liiga's codes for the team's short-handed spells in the period, one per spell (code meaning undocumented). |
| `short_handed_goals_against` | integer | Short handed goals against (team, this period). |
| `penalty_minutes` | integer | Penalty minutes (team, this period). |
| `face_off_wins` | integer | Face off wins (team, this period). |
| `two_minute_penalties` | integer | Two minute penalties (team, this period). |
| `five_minute_penalties` | integer | Five minute penalties (team, this period). |
| `ten_minute_penalties` | integer | Ten minute penalties (team, this period). |
| `twenty_minute_penalties` | integer | Twenty minute penalties (team, this period). |
| `total_distance_travelled` | numeric | Distance skated by the team in the period (unit not stated). |
| `puck_control_duration` | numeric | Seconds this team controlled the puck in the period. |
| `opponent_puck_control_duration` | numeric | Seconds the other team controlled the puck in the period. |
| `contested_control_duration` | numeric | Seconds the puck was contested in the period. |

## Details

Endpoint: `GET https://liiga.fi/api/v2/games/stats/{season}/{game_id}`

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
[`liiga_games()`](https://fastRhockey.sportsdataverse.org/reference/liiga_games.md),
[`liiga_gameweeks()`](https://fastRhockey.sportsdataverse.org/reference/liiga_gameweeks.md),
[`liiga_goals()`](https://fastRhockey.sportsdataverse.org/reference/liiga_goals.md),
[`liiga_player()`](https://fastRhockey.sportsdataverse.org/reference/liiga_player.md),
[`liiga_player_career()`](https://fastRhockey.sportsdataverse.org/reference/liiga_player_career.md),
[`liiga_player_game_log()`](https://fastRhockey.sportsdataverse.org/reference/liiga_player_game_log.md),
[`liiga_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/liiga_player_stats.md),
[`liiga_players_info()`](https://fastRhockey.sportsdataverse.org/reference/liiga_players_info.md),
[`liiga_schedule()`](https://fastRhockey.sportsdataverse.org/reference/liiga_schedule.md),
[`liiga_shotmap()`](https://fastRhockey.sportsdataverse.org/reference/liiga_shotmap.md),
[`liiga_standings()`](https://fastRhockey.sportsdataverse.org/reference/liiga_standings.md),
[`liiga_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/liiga_team_stats.md),
[`liiga_teams()`](https://fastRhockey.sportsdataverse.org/reference/liiga_teams.md),
[`liiga_teams_history()`](https://fastRhockey.sportsdataverse.org/reference/liiga_teams_history.md),
[`liiga_tournament()`](https://fastRhockey.sportsdataverse.org/reference/liiga_tournament.md)

## Examples

``` r
# \donttest{
  try(liiga_game_team_stats(game_id = 1, season = 2026))
#> ── Liiga game team stats Information from liiga.fi ─── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-13 01:55:19 UTC
#> # A tibble: 10 × 22
#>    season game_id side  period   team_id team_slug goals shots
#>     <int>   <int> <chr>  <int>     <int> <chr>     <int> <int>
#>  1   2026       1 home       1  55786244 hpk           1     9
#>  2   2026       1 home       2  55786244 hpk           0    12
#>  3   2026       1 home       3  55786244 hpk           1     9
#>  4   2026       1 home       4  55786244 hpk           0     8
#>  5   2026       1 home       5  55786244 hpk           0    NA
#>  6   2026       1 away       1 651304385 tps           1    24
#>  7   2026       1 away       2 651304385 tps           0    12
#>  8   2026       1 away       3 651304385 tps           1    15
#>  9   2026       1 away       4 651304385 tps           0     1
#> 10   2026       1 away       5 651304385 tps           0    NA
#> # ℹ 14 more variables: power_play_instances <chr>, power_play_goals <int>,
#> #   short_handed_instances <chr>, short_handed_goals_against <int>,
#> #   penalty_minutes <int>, face_off_wins <int>, two_minute_penalties <int>,
#> #   five_minute_penalties <int>, ten_minute_penalties <int>,
#> #   twenty_minute_penalties <int>, total_distance_travelled <dbl>,
#> #   puck_control_duration <dbl>, opponent_puck_control_duration <dbl>,
#> #   contested_control_duration <dbl>
# }
```
