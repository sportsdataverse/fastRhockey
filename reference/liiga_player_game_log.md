# **Liiga player game log**

One player's game log for a season, every tournament.

## Usage

``` r
liiga_player_game_log(player_id, season)
```

## Arguments

- player_id:

  Player id (player_id in liiga_players_info or a lineup; equals
  fihaId), e.g. 21870028.

- season:

  Season END year (2026 = 2025-26).

## Value

A `fastRhockey_data` tibble (42 columns; zero rows when a valid payload
has none).

|  |  |  |
|----|----|----|
| col_name | type | description |
| `player_id` | integer | Player id; the same number in lineups, rosters, stat tables and the profile (fihaId). |
| `season` | integer | Season END year (2026 = 2025-26). |
| `tournament` | character | Tournament of the game (the body's list key): regular, playoffs, practice, qualifications, playout, chl or pitsiturnaus. |
| `game_str` | character | Matchup label, e.g. `HPK - LUK`. |
| `game_id` | integer | Game id, unique only within a season: key a game as (season, game_id). |
| `date` | character | Game start, ISO 8601 UTC. |
| `assists` | integer | Assists (player, this game). |
| `blocked_or_saved_shots` | integer | Blocked or saved shots (player, this game). |
| `faceoffs_total` | integer | Faceoffs total (player, this game). |
| `faceoffs_won` | integer | Faceoffs won (player, this game). |
| `faceoffs_percentage` | numeric | Faceoffs percentage (player, this game). Percent, 0-100, sent as a STRING. |
| `goals` | integer | Goals (player, this game). |
| `minus` | integer | On ice for goals against (the - of plus/minus). |
| `total_points` | integer | Total points (player, this game). |
| `home_team_name` | character | Display name of the home team of the game, e.g. HPK. |
| `away_team_name` | character | Display name of the away team of the game, e.g. Lukko. |
| `penalty_minutes` | integer | Penalty minutes (player, this game). |
| `penaltykill_assists` | integer | Penaltykill assists (player, this game). |
| `penaltykill_goals` | integer | Penaltykill goals (player, this game). |
| `penaltykill_points` | integer | Penaltykill points (player, this game). |
| `plus` | integer | On ice for goals for (the + of plus/minus). |
| `plus_minus` | integer | Plus/minus: on-ice goals for minus on-ice goals against. |
| `powerplay_assists` | integer | Powerplay assists (player, this game). |
| `powerplay_goals` | integer | Powerplay goals (player, this game). |
| `powerplay_seconds` | integer | Powerplay seconds (player, this game). Seconds. |
| `time_on_ice` | integer | Time on ice (player, this game). Seconds. |
| `saves` | integer | Saves (player, this game). |
| `save_percentage` | numeric | Save percentage (player, this game). Percent, 0-100, sent as a STRING. |
| `block_or_saved_shots_percentage` | numeric | Block or saved shots percentage (player, this game). Percent, 0-100, sent as a STRING. |
| `shots` | integer | Shots (player, this game). |
| `shots_into_goal` | integer | Shots into goal (player, this game). |
| `shut_out` | integer | 1 for a goalkeeper's shutout in this game, else 0. |
| `shots_saved` | integer | Shots saved (player, this game). |
| `shots_saved_by_skater` | integer | Shots saved by skater (player, this game). |
| `shot_percentage` | numeric | Shot percentage (player, this game). Percent, 0-100, sent as a STRING. |
| `shot_into_goal_percentage` | numeric | Shot into goal percentage (player, this game). Percent, 0-100, sent as a STRING. |
| `goals_against` | integer | Goals against (player, this game). |
| `goalkeeper` | logical | True when the player is a goalkeeper. |
| `winning_goals` | integer | Winning goals (player, this game). |
| `won` | integer | 1 when the player's team won the game, else 0. |
| `tied` | integer | 1 when Liiga counts the game as a tie for the player's team, else 0 (overtime handling unverified). |
| `lost` | integer | 1 when the player's team lost the game, else 0. |

## Details

Endpoint:
`GET https://liiga.fi/api/v2/players/info/{player_id}/games/{season}`

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
  try(liiga_player_game_log(player_id = 21870028, season = 2026))
#> ── Liiga player game log Information from liiga.fi ─── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-30 00:15:33 UTC
#> # A tibble: 76 × 42
#>    player_id season tournament game_str  game_id date                 assists
#>        <int>  <int> <chr>      <chr>       <int> <chr>                  <int>
#>  1  21870028   2026 practice   LUK - HPK    1553 2025-08-15T12:00:00Z       0
#>  2  21870028   2026 practice   TPS - HPK    1556 2025-08-16T14:00:00Z       0
#>  3  21870028   2026 practice   IFK - HPK    1731 2025-08-21T15:00:00Z       1
#>  4  21870028   2026 practice   HPK - KES    2380 2025-08-27T15:30:00Z       0
#>  5  21870028   2026 practice   HPK - VAX    2382 2025-08-28T15:30:00Z       0
#>  6  21870028   2026 practice   HPK - IFK    1733 2025-09-02T15:30:00Z       1
#>  7  21870028   2026 practice   HPK - TAP    1547 2025-09-04T15:30:00Z       1
#>  8  21870028   2026 regular    HPK - TPS       1 2025-09-09T15:30:00Z       0
#>  9  21870028   2026 regular    ÄSS - HPK      13 2025-09-12T15:30:00Z       1
#> 10  21870028   2026 regular    HPK - SPO      15 2025-09-13T14:00:00Z       0
#> # ℹ 66 more rows
#> # ℹ 35 more variables: blocked_or_saved_shots <int>, faceoffs_total <int>,
#> #   faceoffs_won <int>, faceoffs_percentage <dbl>, goals <int>, minus <int>,
#> #   total_points <int>, home_team_name <chr>, away_team_name <chr>,
#> #   penalty_minutes <int>, penaltykill_assists <int>, penaltykill_goals <int>,
#> #   penaltykill_points <int>, plus <int>, plus_minus <int>,
#> #   powerplay_assists <int>, powerplay_goals <int>, powerplay_seconds <int>, …
# }
```
