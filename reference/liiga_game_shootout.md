# **Liiga game shootout**

One game's shootout attempts: shooter, goalkeeper and result (none for a
game without one).

## Usage

``` r
liiga_game_shootout(season, game_id)
```

## Arguments

- season:

  Season END year (2026 = 2025-26). Game ids repeat across seasons, so a
  game is (season, game_id).

- game_id:

  Game id (game_id in liiga_schedule or liiga_games), e.g. 1; unique
  only within a season.

## Value

A `fastRhockey_data` tibble (11 columns; zero rows when a valid payload
has none).

|  |  |  |
|----|----|----|
| col_name | type | description |
| `season` | integer | Season END year (2026 = 2025-26). |
| `game_id` | integer | Game id, unique only within a season: key a game as (season, game_id). |
| `shooting_player_id` | integer | Shooter's player id. |
| `blocking_player_id` | integer | Goalkeeper's player id. |
| `shot_number` | integer | Attempt number in the shootout. |
| `shooting_team_id` | integer | Team id of the shooter's team (the number of the `<num>:<slug>` key). |
| `shooting_team_slug` | character | Slug of the shooter's team (e.g. hpk). |
| `blocking_team_id` | integer | Goalkeeper's team id. |
| `blocking_team_slug` | character | Goalkeeper's team slug. |
| `goal_scored` | logical | The attempt scored. |
| `winning_goal` | logical | Game-winning goal. |

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
[`liiga_game_penalties()`](https://fastRhockey.sportsdataverse.org/reference/liiga_game_penalties.md),
[`liiga_game_players_stats()`](https://fastRhockey.sportsdataverse.org/reference/liiga_game_players_stats.md),
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
  try(liiga_game_shootout(game_id = 1, season = 2026))
#> ── Liiga game shootout Information from liiga.fi ───── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 05:47:22 UTC
#> # A tibble: 6 × 11
#>   season game_id shooting_player_id blocking_player_id shot_number
#>    <int>   <int>              <int>              <int>       <int>
#> 1   2026       1           34320235           30117507           1
#> 2   2026       1           60900264           40107053           2
#> 3   2026       1           30851669           30117507           3
#> 4   2026       1           27023853           40107053           4
#> 5   2026       1           29885743           30117507           5
#> 6   2026       1           31569277           40107053           6
#> # ℹ 6 more variables: shooting_team_id <int>, shooting_team_slug <chr>,
#> #   blocking_team_id <int>, blocking_team_slug <chr>, goal_scored <lgl>,
#> #   winning_goal <lgl>
# }
```
