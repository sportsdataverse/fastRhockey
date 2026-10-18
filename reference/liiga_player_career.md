# **Liiga player career**

One player's career: season totals by tournament.

## Usage

``` r
liiga_player_career(player_id)
```

## Arguments

- player_id:

  Player id (player_id in liiga_players_info or a lineup; equals
  fihaId), e.g. 21870028.

## Value

A `fastRhockey_data` tibble (39 columns; zero rows when a valid payload
has none).

|  |  |  |
|----|----|----|
| col_name | type | description |
| `player_id` | integer | Player id; the same number in lineups, rosters, stat tables and the profile (fihaId). |
| `season` | integer | Season END year (2026 = 2025-26). |
| `games` | integer | Games (player, that season and tournament). |
| `played_games` | integer | Played games (player, that season and tournament). |
| `goals` | integer | Goals (player, that season and tournament). |
| `assists` | integer | Assists (player, that season and tournament). |
| `points` | integer | Points (player, that season and tournament). |
| `penalty_minutes` | integer | Penalty minutes (player, that season and tournament). |
| `plus` | integer | On ice for goals for (the + of plus/minus). |
| `minus` | integer | On ice for goals against (the - of plus/minus). |
| `plus_minus` | integer | Plus/minus: on-ice goals for minus on-ice goals against. |
| `power_play_goals` | integer | Power play goals (player, that season and tournament). |
| `short_handed_goals` | integer | Short handed goals (player, that season and tournament). |
| `winning_goals` | integer | Winning goals (player, that season and tournament). |
| `shots` | integer | Shots (player, that season and tournament). |
| `shots_into_goal` | integer | Shots into goal (player, that season and tournament). |
| `shot_percentage` | numeric | Shot percentage (player, that season and tournament). Percent, 0-100, sent as a STRING. |
| `shot_into_goal_percentage` | numeric | Shot into goal percentage (player, that season and tournament). Percent, 0-100, sent as a STRING. |
| `tournament` | character | Tournament of the career row, uppercase: RUNKOSARJA (regular season), PLAYOFFS, PRACTICE (preseason), ... |
| `league_name` | character | Name of the league of that career row, e.g. Liiga. |
| `team_name` | character | Team's display name, e.g. HPK or Tappara. |
| `end_time` | character | ISO 8601 UTC timestamp, not game seconds: the end of that season-tournament for finished seasons (2026-03-17T23:59:59Z for the 2025-26 regular season) and the request time for the season in progress. Looks like the end of the stats window; meaning unverified. |
| `team_short_name` | character | Team abbreviation. |
| `time_on_ice` | integer | Time on ice (player, that season and tournament). Seconds. |
| `save_percentage` | numeric | Save percentage (player, that season and tournament). Percent, 0-100, sent as a STRING. |
| `goals_against_average` | numeric | Goals against per game (goalkeeper), that season and tournament; sent as a string. |
| `win_percentage` | numeric | Win percentage (player, that season and tournament). Percent, 0-100, sent as a STRING. |
| `shut_out` | character | Shutouts (goalkeeper), that season and tournament; sent as a string (e.g. "0"). |
| `faceoffs_won` | integer | Faceoffs won (player, that season and tournament). |
| `faceoffs_percentage` | numeric | Faceoffs percentage (player, that season and tournament). Percent, 0-100, sent as a STRING. |
| `wins` | integer | Wins (player, that season and tournament). |
| `losses` | integer | Losses (player, that season and tournament). |
| `ties` | integer | Ties (player, that season and tournament). |
| `gk_wins` | integer | Goalkeeper wins (player, that season and tournament). |
| `gk_losses` | integer | Goalkeeper losses (player, that season and tournament). |
| `gk_ties` | integer | Goalkeeper ties (player, that season and tournament). |
| `saves` | integer | Saves (player, that season and tournament). |
| `goals_against` | integer | Goals against (player, that season and tournament). |
| `faceoff_total` | integer | Faceoff total (player, that season and tournament). |

## Details

Endpoint: `GET https://liiga.fi/api/v2/players/info/{player_id}`

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
  try(liiga_player_career(player_id = 21870028))
#> ── Liiga player career Information from liiga.fi ───── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 12:06:51 UTC
#> # A tibble: 29 × 39
#>    player_id season games played_games goals assists points penalty_minutes
#>        <int>  <int> <int>        <int> <int>   <int>  <int>           <int>
#>  1  21870028   2027    14           14     0       5      5               4
#>  2  21870028   2026    60           60    12      14     26               6
#>  3  21870028   2025    57           57     5      10     15              18
#>  4  21870028   2024    36           36     3       8     11              10
#>  5  21870028   2023    60           60    10       6     16               6
#>  6  21870028   2022    50           50     8       8     16               6
#>  7  21870028   2021    56           56     7       9     16              18
#>  8  21870028   2019     8            8     2       2      4              18
#>  9  21870028   2019    37           37     2       2      4               4
#> 10  21870028   2018    34           34     2       3      5               6
#> # ℹ 19 more rows
#> # ℹ 31 more variables: plus <int>, minus <int>, plus_minus <int>,
#> #   power_play_goals <int>, short_handed_goals <int>, winning_goals <int>,
#> #   shots <int>, shots_into_goal <int>, shot_percentage <dbl>,
#> #   shot_into_goal_percentage <dbl>, tournament <chr>, league_name <chr>,
#> #   team_name <chr>, end_time <chr>, team_short_name <chr>, time_on_ice <int>,
#> #   save_percentage <dbl>, goals_against_average <dbl>, win_percentage <dbl>, …
# }
```
