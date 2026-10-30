# **Liiga goals**

Every goal of a season and tournament: scorer, assists, strength codes
and skaters on ice.

## Usage

``` r
liiga_goals(season = NULL, tournament = NULL)
```

## Arguments

- season:

  Season END year (2026 = 2025-26). Omitted = the season in progress.

- tournament:

  Tournament: runkosarja (regular season), playoffs, playout,
  qualifications, valmistavat_ottelut (preseason) or chl (Champions
  Hockey League). Omitted = every tournament, preseason included.

## Value

A `fastRhockey_data` tibble (30 columns; zero rows when a valid payload
has none).

|  |  |  |
|----|----|----|
| col_name | type | description |
| `season` | integer | Season END year (2026 = 2025-26). |
| `game_id` | integer | Game id, unique only within a season: key a game as (season, game_id). |
| `side` | character | Which team of the game the row belongs to: home or away. |
| `team_id` | integer | Team id, the number of Liiga's `<num>:<slug>` team key (sent as that string, a bare integer or a digit string depending on the route). |
| `team_slug` | character | Team slug, the part of Liiga's `<num>:<slug>` team key after the colon (e.g. hpk). |
| `log_time` | character | Wall-clock time the event was logged, ISO 8601 UTC. |
| `winning_goal` | logical | Game-winning goal. |
| `game_time` | integer | Seconds from the start of the game. Regular season: 3900 (65:00) = the end of the 5-minute overtime; playoff overtime runs past it. |
| `period` | integer | Period number: 1-3 are regulation, 4 and up overtime (or the shootout). |
| `event_id` | integer | Event id within the game. |
| `home_team_score` | integer | Home score after the event. |
| `away_team_score` | integer | Away score after the event. |
| `goals_so_far_in_season` | integer | Scorer's season goal count after this goal. |
| `video_clip_url` | character | URL of the goal's video clip; null in every capture. |
| `video_clip_playlist_url` | character | URL of the goal's video clip playlist; null in every capture. |
| `video_thumbnail_url` | character | URL of the goal video's thumbnail image; null in every capture. |
| `scorer_player_id` | integer | Scorer's player id. |
| `scorer_first_name` | character | Scorer's first name. |
| `scorer_last_name` | character | Scorer's last name. |
| `assist1_player_id` | integer | First assist's player id. |
| `assist1_first_name` | character | First assist's first name. |
| `assist1_last_name` | character | First assist's last name. |
| `assist1_season_assists` | integer | First assister's season assist count after this goal. |
| `assist2_player_id` | integer | Second assist's player id. |
| `assist2_first_name` | character | Second assist's first name. |
| `assist2_last_name` | character | Second assist's last name. |
| `assist2_season_assists` | integer | Second assister's season assist count after this goal. |
| `goal_types` | character | Goal type codes, comma-separated Finnish abbreviations: YV power play, AV short-handed, TM empty net, RL penalty shot (others undocumented). |
| `plus_jerseys` | character | Jersey NUMBERS of this team's skaters on ice for the goal, space-separated. |
| `minus_jerseys` | character | Jersey NUMBERS of the other team's skaters on ice for the goal, space-separated. |

## Details

Endpoint: `GET https://liiga.fi/api/v2/games`

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
  try(liiga_goals(season = 2026, tournament = "playoffs"))
#> ── Liiga goals Information from liiga.fi ───────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-30 00:15:31 UTC
#> # A tibble: 301 × 30
#>    season game_id side  team_id team_slug log_time winning_goal game_time period
#>     <int>   <int> <chr>   <int> <chr>     <chr>    <lgl>            <int>  <int>
#>  1   2026   55080 home   8.60e8 kalpa     2026-03… FALSE              209      1
#>  2   2026   55080 home   8.60e8 kalpa     2026-03… TRUE               639      1
#>  3   2026   55080 home   8.60e8 kalpa     2026-03… FALSE             2285      2
#>  4   2026   55080 home   8.60e8 kalpa     2026-03… FALSE             2729      3
#>  5   2026   55080 away   1.69e8 hifk      2026-03… FALSE              873      1
#>  6   2026   55081 home   6.79e8 ässät     2026-03… FALSE              165      1
#>  7   2026   55081 home   6.79e8 ässät     2026-03… FALSE             3206      3
#>  8   2026   55081 home   6.79e8 ässät     2026-03… FALSE             3477      3
#>  9   2026   55081 home   6.79e8 ässät     2026-03… TRUE              3881      4
#> 10   2026   55081 away   1.37e9 k-espoo   2026-03… FALSE              440      1
#> # ℹ 291 more rows
#> # ℹ 21 more variables: event_id <int>, home_team_score <int>,
#> #   away_team_score <int>, goals_so_far_in_season <int>, video_clip_url <chr>,
#> #   video_clip_playlist_url <chr>, video_thumbnail_url <chr>,
#> #   scorer_player_id <int>, scorer_first_name <chr>, scorer_last_name <chr>,
#> #   assist1_player_id <int>, assist1_first_name <chr>, assist1_last_name <chr>,
#> #   assist1_season_assists <int>, assist2_player_id <int>, …
# }
```
