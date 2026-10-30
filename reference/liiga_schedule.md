# **Liiga schedule**

Schedule of a season and tournament: game ids, teams, goals, xG, start
and rink.

## Usage

``` r
liiga_schedule(season = NULL, tournament = NULL)
```

## Arguments

- season:

  Season END year (2026 = 2025-26). Omitted = the season in progress.

- tournament:

  Tournament: runkosarja (regular season), playoffs, playout,
  qualifications, valmistavat_ottelut (preseason) or chl (Champions
  Hockey League). Omitted = runkosarja.

## Value

A `fastRhockey_data` tibble (35 columns; zero rows when a valid payload
has none).

|  |  |  |
|----|----|----|
| col_name | type | description |
| `game_id` | integer | Game id, unique only within a season: key a game as (season, game_id). |
| `serie_id` | integer | Tournament id, the same every season: 1188 regular season, 1189 playoffs, 1194 preseason, 1890 CHL. |
| `season` | integer | Season END year (2026 = 2025-26). |
| `home_team_name` | character | Display name of the home team, e.g. HPK. |
| `away_team_name` | character | Display name of the away team, e.g. TPS. |
| `home_team_id` | integer | Home team id (the number of the `<num>:<slug>` key). |
| `home_team_slug` | character | Home team slug (e.g. hpk). |
| `away_team_id` | integer | Away team id (the number of the `<num>:<slug>` key). |
| `away_team_slug` | character | Away team slug (e.g. tps). |
| `home_team_placeholder` | character | Placeholder label for an undecided playoff home team, else null. |
| `away_team_placeholder` | character | Placeholder label for an undecided playoff away team, else null. |
| `home_team_goals` | integer | Goals scored by the home team in the game. |
| `away_team_goals` | integer | Goals scored by the away team in the game. |
| `expected_home_team_goals` | numeric | Home team's expected goals (xG) for the game, provider-computed (same value as /games `homeTeam.expectedGoals`). |
| `expected_away_team_goals` | numeric | Away team's expected goals (xG) for the game, provider-computed. |
| `game_time` | integer | Seconds from the start of the game. Regular season: 3900 (65:00) = the end of the 5-minute overtime; playoff overtime runs past it. |
| `started` | logical | Game has started. |
| `ended` | logical | Game has ended. |
| `home_team_winner` | logical | True when the home team won the game. |
| `spectators` | integer | Attendance (spectators) reported for the game. |
| `home_team_ranking` | integer | Home team's table position; null in every capture. |
| `away_team_ranking` | integer | Away team's table position; null in every capture. |
| `play_off_pair` | integer | Playoff pairing number, else null. |
| `play_off_phase` | integer | Playoff round, else null. |
| `play_off_req_wins` | integer | Wins needed to take the series, else null. |
| `start` | character | Scheduled start, ISO 8601 UTC. |
| `buy_tickets_url` | character | Ticket link, else null. |
| `finished_type` | character | How the game ended: ENDED_DURING_REGULAR_GAME_TIME, ENDED_DURING_EXTENDED_GAME_TIME (overtime), ENDED_DURING_WINNING_SHOT_COMPETITION (shootout). |
| `ice_rink_id` | integer | Liiga's id of the rink (ice arena). |
| `ice_rink_name` | character | Name of the rink (ice arena). |
| `ice_rink_latitude` | numeric | Rink latitude, decimal degrees. |
| `ice_rink_longitude` | numeric | Rink longitude, decimal degrees. |
| `ice_rink_street_address` | character | Street address of the rink. |
| `ice_rink_zip` | character | Postal code of the rink. |
| `ice_rink_city` | character | City where the rink is located. |

## Details

Endpoint: `GET https://liiga.fi/api/v2/schedule`

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
[`liiga_shotmap()`](https://fastRhockey.sportsdataverse.org/reference/liiga_shotmap.md),
[`liiga_standings()`](https://fastRhockey.sportsdataverse.org/reference/liiga_standings.md),
[`liiga_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/liiga_team_stats.md),
[`liiga_teams()`](https://fastRhockey.sportsdataverse.org/reference/liiga_teams.md),
[`liiga_teams_history()`](https://fastRhockey.sportsdataverse.org/reference/liiga_teams_history.md),
[`liiga_tournament()`](https://fastRhockey.sportsdataverse.org/reference/liiga_tournament.md)

## Examples

``` r
# \donttest{
  try(liiga_schedule(season = 2026, tournament = "runkosarja"))
#> ── Liiga schedule Information from liiga.fi ────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-30 00:15:37 UTC
#> # A tibble: 480 × 35
#>    game_id serie_id season home_team_name away_team_name home_team_id
#>      <int>    <int>  <int> <chr>          <chr>                 <int>
#>  1       1     1188   2026 HPK            TPS                55786244
#>  2       2     1188   2026 Jukurit        HIFK              292293444
#>  3       3     1188   2026 KooKoo         Ässät             461765763
#>  4       4     1188   2026 Kärpät         Sport             495643563
#>  5       6     1188   2026 Pelicans       K-Espoo           875886777
#>  6       8     1188   2026 SaiPa          KalPa             933686567
#>  7       5     1188   2026 Lukko          Tappara           624554857
#>  8       7     1188   2026 JYP            Ilves             219244634
#>  9       9     1188   2026 JYP            Jukurit           219244634
#> 10      10     1188   2026 Kärpät         Ilves             495643563
#> # ℹ 470 more rows
#> # ℹ 29 more variables: home_team_slug <chr>, away_team_id <int>,
#> #   away_team_slug <chr>, home_team_placeholder <chr>,
#> #   away_team_placeholder <chr>, home_team_goals <int>, away_team_goals <int>,
#> #   expected_home_team_goals <dbl>, expected_away_team_goals <dbl>,
#> #   game_time <int>, started <lgl>, ended <lgl>, home_team_winner <lgl>,
#> #   spectators <int>, home_team_ranking <int>, away_team_ranking <int>, …
# }
```
