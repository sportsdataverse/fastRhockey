# **Liiga team history**

Every team's regular-season record, one row per team and season since
1976.

## Usage

``` r
liiga_teams_history()
```

## Value

A `fastRhockey_data` tibble (25 columns; zero rows when a valid payload
has none).

|  |  |  |
|----|----|----|
| col_name | type | description |
| `id` | integer | Row id of this team-season row (NOT a team id; `teamId` is the team). |
| `season` | integer | Season END year (2026 = 2025-26). |
| `league_id` | integer | League id: 1 = Liiga (SM-liiga) on every /teams/info row 1976-2027. |
| `serie_id` | integer | Tournament id, the same every season: 1188 regular season, 1189 playoffs, 1194 preseason, 1890 CHL. |
| `team_id` | integer | Team id, the number of Liiga's `<num>:<slug>` team key (sent as that string, a bare integer or a digit string depending on the route). |
| `away_games` | integer | Away games (team, that regular season). |
| `away_losses` | integer | Away losses (team, that regular season). |
| `away_ties` | integer | Away ties (team, that regular season). |
| `away_wins` | integer | Away wins (team, that regular season). |
| `away_wins_in_extra_time` | integer | Away wins in extra time (team, that regular season). |
| `games` | integer | Games (team, that regular season). |
| `goals_against` | integer | Goals against (team, that regular season). |
| `goals_scored` | integer | Goals scored (team, that regular season). |
| `home_games` | integer | Home games (team, that regular season). |
| `home_losses` | integer | Home losses (team, that regular season). |
| `home_ties` | integer | Home ties (team, that regular season). |
| `home_wins` | integer | Home wins (team, that regular season). |
| `home_wins_in_extra_time` | integer | Home wins in extra time (team, that regular season). |
| `losses` | integer | Losses (team, that regular season). |
| `playoff_rank` | integer | Playoff rank (team, that regular season). |
| `points` | integer | Points (team, that regular season). |
| `regular_season_rank` | integer | Regular season rank (team, that regular season). |
| `ties` | integer | Ties (team, that regular season). |
| `wins` | integer | Wins (team, that regular season). |
| `wins_in_extra_time` | integer | Wins in extra time (team, that regular season). |

## Details

Endpoint: `GET https://liiga.fi/api/v2/teams/info` (with `team=all`)

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
[`liiga_shotmap()`](https://fastRhockey.sportsdataverse.org/reference/liiga_shotmap.md),
[`liiga_standings()`](https://fastRhockey.sportsdataverse.org/reference/liiga_standings.md),
[`liiga_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/liiga_team_stats.md),
[`liiga_teams()`](https://fastRhockey.sportsdataverse.org/reference/liiga_teams.md),
[`liiga_tournament()`](https://fastRhockey.sportsdataverse.org/reference/liiga_tournament.md)

## Examples

``` r
# \donttest{
  try(liiga_teams_history())
#> ── Liiga team history Information from liiga.fi ────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-30 00:15:53 UTC
#> # A tibble: 593 × 25
#>       id season league_id serie_id    team_id away_games away_losses away_ties
#>    <int>  <int>     <int>    <int>      <int>      <int>       <int>     <int>
#>  1   848   2027         1     1188  624554857          9           0         3
#>  2   850   2027         1     1188  933686567          8           3         0
#>  3   843   2027         1     1188  292293444          8           3         1
#>  4   847   2027         1     1188  495643563          9           2         5
#>  5   841   2027         1     1188  951626834         11           5         1
#>  6   844   2027         1     1188  219244634         10           3         0
#>  7   853   2027         1     1188  651304385         10           5         3
#>  8   854   2027         1     1188  679171680         10           4         2
#>  9   852   2027         1     1188  362185137         10           4         2
#> 10   855   2027         1     1188 1368624751          8           4         4
#> # ℹ 583 more rows
#> # ℹ 17 more variables: away_wins <int>, away_wins_in_extra_time <int>,
#> #   games <int>, goals_against <int>, goals_scored <int>, home_games <int>,
#> #   home_losses <int>, home_ties <int>, home_wins <int>,
#> #   home_wins_in_extra_time <int>, losses <int>, points <int>,
#> #   playoff_rank <int>, regular_season_rank <int>, ties <int>, wins <int>,
#> #   wins_in_extra_time <int>
# }
```
