# **Liiga standings**

Standings of a season: regular season, playoffs, preseason, playout and
qualifications tables.

## Usage

``` r
liiga_standings(season = NULL)
```

## Arguments

- season:

  Season END year (2026 = 2025-26). Omitted = the season in progress.

## Value

A `fastRhockey_data` tibble (49 columns; zero rows when a valid payload
has none).

|  |  |  |
|----|----|----|
| col_name | type | description |
| `season` | integer | Season END year (2026 = 2025-26). |
| `tournament` | character | Table the row belongs to, in the request vocabulary: runkosarja, playoffs, valmistavat_ottelut, playout or qualifications. |
| `internal_id` | integer | Team id (same number as team_id). |
| `team_id` | integer | Team id, the number of Liiga's `<num>:<slug>` team key (sent as that string, a bare integer or a digit string depending on the route). |
| `team_slug` | character | Team slug, the part of Liiga's `<num>:<slug>` team key after the colon (e.g. hpk). |
| `team_name` | character | Team's display name, e.g. HPK or Tappara. |
| `team_logos_dark_bg` | character | Logo image URL for dark backgrounds. |
| `team_logos_light_bg` | character | Logo image URL for light backgrounds. |
| `team_logos_dark_bg_original` | character | Original-file logo URL for dark backgrounds; null in every capture. |
| `team_logos_light_bg_original` | character | Original-file logo URL for light backgrounds; null in every capture. |
| `ranking` | integer | Table position. |
| `live_ranking` | integer | Table position counting games in progress. |
| `games` | integer | Games (team, season to date in that table's tournament). |
| `live_games` | integer | Games counting games in progress (team, season to date in that table's tournament). |
| `wins` | integer | Wins (team, season to date in that table's tournament). |
| `live_wins` | integer | Wins counting games in progress (team, season to date in that table's tournament). |
| `overtime_wins` | integer | Overtime wins (team, season to date in that table's tournament). |
| `losses` | integer | Losses (team, season to date in that table's tournament). |
| `live_losses` | integer | Losses counting games in progress (team, season to date in that table's tournament). |
| `ties` | integer | Ties (team, season to date in that table's tournament). |
| `live_ties` | integer | Ties counting games in progress (team, season to date in that table's tournament). |
| `overtime_losses` | integer | Overtime losses (team, season to date in that table's tournament). |
| `points` | integer | Points (team, season to date in that table's tournament). |
| `live_points` | integer | Points counting games in progress (team, season to date in that table's tournament). |
| `goals` | integer | Goals (team, season to date in that table's tournament). |
| `goals_against` | integer | Goals against (team, season to date in that table's tournament). |
| `live_goals` | integer | Goals counting games in progress (team, season to date in that table's tournament). |
| `live_goals_against` | integer | Goals against counting games in progress (team, season to date in that table's tournament). |
| `power_play_instances` | integer | Power play instances (team, season to date in that table's tournament). |
| `power_play_time` | integer | Seconds on the power play, season to date. |
| `power_play_goals` | integer | Power play goals (team, season to date in that table's tournament). |
| `short_handed_instances` | integer | Short handed instances (team, season to date in that table's tournament). |
| `short_handed_time` | integer | Seconds short-handed, season to date. |
| `short_handed_goals_against` | integer | Short handed goals against (team, season to date in that table's tournament). |
| `penalty_minutes` | integer | Penalty minutes (team, season to date in that table's tournament). |
| `two_minute_penalties` | integer | Two minute penalties (team, season to date in that table's tournament). |
| `five_minute_penalties` | integer | Five minute penalties (team, season to date in that table's tournament). |
| `ten_minute_penalties` | integer | Ten minute penalties (team, season to date in that table's tournament). |
| `twenty_minute_penalties` | integer | Twenty minute penalties (team, season to date in that table's tournament). |
| `twenty_five_minute_penalties` | integer | Twenty five minute penalties (team, season to date in that table's tournament). |
| `total_penalties` | integer | Total penalties (team, season to date in that table's tournament). |
| `distance` | numeric | Distance (team, season to date in that table's tournament). Distance; unit not stated (unverified). |
| `serie` | character | Tournament code, uppercase (RUNKOSARJA, PLAYOFFS, PRACTICE, ...); in /schedule the integer tournament id instead (see serieId and Divergence notes). |
| `live_points_per_game` | numeric | Points per game counting games in progress (team, season to date in that table's tournament). |
| `power_play_percentage` | numeric | Power-play conversion, a 0-1 FRACTION sent as a STRING (e.g. "0.277"), season to date. |
| `short_handed_percentage` | numeric | Penalty-kill success, a 0-1 FRACTION sent as a STRING (e.g. "0.815"), season to date. |
| `distance_per_game` | numeric | Distance per game (team, season to date in that table's tournament). Distance; unit not stated (unverified). |
| `points_per_game` | numeric | Points per game (team, season to date in that table's tournament). |
| `win_percentage` | numeric | Share of games won, a 0-1 FRACTION sent as a STRING (e.g. "0.583"), season to date. |

## Details

Endpoint: `GET https://liiga.fi/api/v2/standings/`

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
[`liiga_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/liiga_team_stats.md),
[`liiga_teams()`](https://fastRhockey.sportsdataverse.org/reference/liiga_teams.md),
[`liiga_teams_history()`](https://fastRhockey.sportsdataverse.org/reference/liiga_teams_history.md),
[`liiga_tournament()`](https://fastRhockey.sportsdataverse.org/reference/liiga_tournament.md)

## Examples

``` r
# \donttest{
  try(liiga_standings(season = 2026))
#> ── Liiga standings Information from liiga.fi ───────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 05:47:39 UTC
#> # A tibble: 54 × 49
#>    season tournament internal_id  team_id team_slug team_name team_logos_dark_bg
#>     <int> <chr>            <int>    <int> <chr>     <chr>     <chr>             
#>  1   2026 runkosarja   362185137   3.62e8 tappara   Tappara   https://cdn.build…
#>  2   2026 runkosarja   461765763   4.62e8 kookoo    KooKoo    https://cdn.build…
#>  3   2026 runkosarja   933686567   9.34e8 saipa     SaiPa     https://cdn.build…
#>  4   2026 runkosarja   951626834   9.52e8 ilves     Ilves     https://cdn.build…
#>  5   2026 runkosarja   624554857   6.25e8 lukko     Lukko     https://cdn.build…
#>  6   2026 runkosarja   219244634   2.19e8 jyp       JYP       https://cdn.build…
#>  7   2026 runkosarja   859884935   8.60e8 kalpa     KalPa     https://cdn.build…
#>  8   2026 runkosarja   679171680   6.79e8 ässät     Ässät     https://cdn.build…
#>  9   2026 runkosarja  1368624751   1.37e9 k-espoo   K-Espoo   https://cdn.build…
#> 10   2026 runkosarja   168761288   1.69e8 hifk      HIFK      https://cdn.build…
#> # ℹ 44 more rows
#> # ℹ 42 more variables: team_logos_light_bg <chr>,
#> #   team_logos_dark_bg_original <chr>, team_logos_light_bg_original <chr>,
#> #   ranking <int>, live_ranking <int>, games <int>, live_games <int>,
#> #   wins <int>, live_wins <int>, overtime_wins <int>, losses <int>,
#> #   live_losses <int>, ties <int>, live_ties <int>, overtime_losses <int>,
#> #   points <int>, live_points <int>, goals <int>, goals_against <int>, …
# }
```
