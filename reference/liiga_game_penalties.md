# **Liiga game penalties**

One game's penalties, both teams: player, fault, minutes, start and end.

## Usage

``` r
liiga_game_penalties(season, game_id)
```

## Arguments

- season:

  Season END year (2026 = 2025-26). Game ids repeat across seasons, so a
  game is (season, game_id).

- game_id:

  Game id (game_id in liiga_schedule or liiga_games), e.g. 1; unique
  only within a season.

## Value

A `fastRhockey_data` tibble (17 columns; zero rows when a valid payload
has none).

|  |  |  |
|----|----|----|
| col_name | type | description |
| `season` | integer | Season END year (2026 = 2025-26). |
| `game_id` | integer | Game id, unique only within a season: key a game as (season, game_id). |
| `side` | character | Which team of the game the row belongs to: home or away. |
| `team_id` | integer | Team id, the number of Liiga's `<num>:<slug>` team key (sent as that string, a bare integer or a digit string depending on the route). |
| `team_slug` | character | Team slug, the part of Liiga's `<num>:<slug>` team key after the colon (e.g. hpk). |
| `player_id` | integer | Player id; the same number in lineups, rosters, stat tables and the profile (fihaId). |
| `sufferer_player_id` | integer | Player who served the penalty. |
| `event_id` | integer | Event id within the game. |
| `log_time` | character | Wall-clock time the event was logged, ISO 8601 UTC. |
| `game_time` | integer | Seconds from the start of the game. Regular season: 3900 (65:00) = the end of the 5-minute overtime; playoff overtime runs past it. |
| `period` | integer | Period number: 1-3 are regulation, 4 and up overtime (or the shootout). |
| `penalty_begintime` | integer | Penalty start, seconds from game start. |
| `penalty_endtime` | integer | Penalty end, seconds from game start. |
| `penalty_fault_name` | character | Infraction, in Finnish. |
| `penalty_fault_type` | character | Infraction code. |
| `penalty_info` | character | Extra penalty code (” or a letter such as K; meaning unverified). |
| `penalty_minutes` | integer | Length of the penalty in minutes (2, 5, 10, 20, ...). |

## Details

Endpoint: `GET https://liiga.fi/api/v2/games/{season}/{game_id}`

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
[`liiga_shotmap()`](https://fastRhockey.sportsdataverse.org/reference/liiga_shotmap.md),
[`liiga_standings()`](https://fastRhockey.sportsdataverse.org/reference/liiga_standings.md),
[`liiga_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/liiga_team_stats.md),
[`liiga_teams()`](https://fastRhockey.sportsdataverse.org/reference/liiga_teams.md),
[`liiga_teams_history()`](https://fastRhockey.sportsdataverse.org/reference/liiga_teams_history.md),
[`liiga_tournament()`](https://fastRhockey.sportsdataverse.org/reference/liiga_tournament.md)

## Examples

``` r
# \donttest{
  try(liiga_game_penalties(game_id = 1, season = 2026))
#> ── Liiga game penalties Information from liiga.fi ──── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 12:06:38 UTC
#> # A tibble: 11 × 17
#>    season game_id side   team_id team_slug player_id sufferer_player_id event_id
#>     <int>   <int> <chr>    <int> <chr>         <int>              <int>    <int>
#>  1   2026       1 home    5.58e7 hpk        60954495           60954495      100
#>  2   2026       1 home    5.58e7 hpk        30851669           30851669      125
#>  3   2026       1 home    5.58e7 hpk        40115108           40115108      130
#>  4   2026       1 home    5.58e7 hpk        31585390           31585390      162
#>  5   2026       1 home    5.58e7 hpk        22709389           22709389      289
#>  6   2026       1 home    5.58e7 hpk               0           40039427      400
#>  7   2026       1 away    6.51e8 tps        25541395           25541395       13
#>  8   2026       1 away    6.51e8 tps        30969140           30969140      163
#>  9   2026       1 away    6.51e8 tps        40048503           40048503      340
#> 10   2026       1 away    6.51e8 tps               0           40373820      387
#> 11   2026       1 away    6.51e8 tps        60953550           60953550      496
#> # ℹ 9 more variables: log_time <chr>, game_time <int>, period <int>,
#> #   penalty_begintime <int>, penalty_endtime <int>, penalty_fault_name <chr>,
#> #   penalty_fault_type <chr>, penalty_info <chr>, penalty_minutes <int>
# }
```
