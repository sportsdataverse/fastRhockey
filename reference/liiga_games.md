# **Liiga games**

Every game of a season and tournament: teams, score, xG, power plays,
periods and rink.

## Usage

``` r
liiga_games(season = NULL, tournament = NULL)
```

## Arguments

- season:

  Season END year (2026 = 2025-26). Omitted = the season in progress.

- tournament:

  Tournament: runkosarja (regular season), playoffs, playout,
  qualifications, valmistavat_ottelut (preseason) or chl (Champions
  Hockey League). Omitted = every tournament, preseason included.

## Value

A `fastRhockey_data` tibble (59 columns; zero rows when a valid payload
has none).

|  |  |  |
|----|----|----|
| col_name | type | description |
| `game_id` | integer | Game id, unique only within a season: key a game as (season, game_id). |
| `season` | integer | Season END year (2026 = 2025-26). |
| `start` | character | Scheduled start, ISO 8601 UTC. |
| `end` | character | Time the game ended, ISO 8601 UTC (absent from the refs captures; seen in the full 2026 playoff body). |
| `periods` | character | JSON list of the game's periods: index, home and away goals, category, start and end second. |
| `finished_type` | character | How the game ended: ENDED_DURING_REGULAR_GAME_TIME, ENDED_DURING_EXTENDED_GAME_TIME (overtime), ENDED_DURING_WINNING_SHOT_COMPETITION (shootout). |
| `started` | logical | Game has started. |
| `ended` | logical | Game has ended. |
| `game_time` | integer | Seconds from the start of the game. Regular season: 3900 (65:00) = the end of the 5-minute overtime; playoff overtime runs past it. |
| `spectators` | integer | Attendance (spectators) reported for the game. |
| `play_off_pair` | integer | Playoff pairing number, else null. |
| `play_off_phase` | integer | Playoff round, else null. |
| `play_off_req_wins` | integer | Wins needed to take the playoff series, else null. |
| `buy_tickets_url` | character | Ticket link for the game, else null. |
| `ice_rink_id` | integer | Liiga's id of the rink (ice arena). |
| `ice_rink_name` | character | Name of the rink (ice arena). |
| `ice_rink_latitude` | numeric | Rink latitude, decimal degrees. |
| `ice_rink_longitude` | numeric | Rink longitude, decimal degrees. |
| `ice_rink_street_address` | character | Street address of the rink. |
| `ice_rink_zip` | character | Postal code of the rink. |
| `ice_rink_city` | character | City where the rink is located. |
| `current_period` | integer | Last period played (3 = regulation; 4 and up = overtime periods or the shootout; 2026 values 3, 4, 5, 7). |
| `stale` | logical | Live data is stale. |
| `serie` | character | Tournament code, uppercase (RUNKOSARJA, PLAYOFFS, PRACTICE, ...); in /schedule the integer tournament id instead (see serieId and Divergence notes). |
| `game_week` | integer | Game week of the season. |
| `home_team_id` | integer | Home team id (the number of the `<num>:<slug>` key). |
| `home_team_slug` | character | Home team slug (e.g. hpk). |
| `home_placeholder` | character | Home team: placeholder label for an undecided playoff team, else null. |
| `home_name` | character | Home team: display name of the team, e.g. HPK. |
| `home_goals` | integer | Home team: goals scored by the team in the game. |
| `home_time_out` | integer | Home team: second of the game (from game start) at which the team called its timeout. |
| `home_powerplay_instances` | integer | Home team: power plays the team had in the game. |
| `home_powerplay_goals` | integer | Home team: power-play goals the team scored in the game. |
| `home_short_handed_instances` | integer | Home team: times the team played short-handed in the game. |
| `home_short_handed_goals` | integer | Home team: short-handed goals the team scored in the game. |
| `home_expected_goals` | numeric | Home team: expected goals (xG) for the team in the game, from Liiga's undocumented model. |
| `home_ranking` | integer | Home team: table position. |
| `home_game_start_date_time` | character | Home team: scheduled start, ISO 8601 UTC. |
| `home_logos_dark_bg` | character | Home team: logo image URL for dark backgrounds. |
| `home_logos_light_bg` | character | Home team: logo image URL for light backgrounds. |
| `home_logos_dark_bg_original` | character | Home team: original-file logo URL for dark backgrounds; null in every capture. |
| `home_logos_light_bg_original` | character | Home team: original-file logo URL for light backgrounds; null in every capture. |
| `away_team_id` | integer | Away team id (the number of the `<num>:<slug>` key). |
| `away_team_slug` | character | Away team slug (e.g. tps). |
| `away_placeholder` | character | Away team: placeholder label for an undecided playoff team, else null. |
| `away_name` | character | Away team: display name of the team, e.g. HPK. |
| `away_goals` | integer | Away team: goals scored by the team in the game. |
| `away_time_out` | integer | Away team: second of the game (from game start) at which the team called its timeout. |
| `away_powerplay_instances` | integer | Away team: power plays the team had in the game. |
| `away_powerplay_goals` | integer | Away team: power-play goals the team scored in the game. |
| `away_short_handed_instances` | integer | Away team: times the team played short-handed in the game. |
| `away_short_handed_goals` | integer | Away team: short-handed goals the team scored in the game. |
| `away_expected_goals` | numeric | Away team: expected goals (xG) for the team in the game, from Liiga's undocumented model. |
| `away_ranking` | integer | Away team: table position. |
| `away_game_start_date_time` | character | Away team: scheduled start, ISO 8601 UTC. |
| `away_logos_dark_bg` | character | Away team: logo image URL for dark backgrounds. |
| `away_logos_light_bg` | character | Away team: logo image URL for light backgrounds. |
| `away_logos_dark_bg_original` | character | Away team: original-file logo URL for dark backgrounds; null in every capture. |
| `away_logos_light_bg_original` | character | Away team: original-file logo URL for light backgrounds; null in every capture. |

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
  try(liiga_games(season = 2026, tournament = "playoffs"))
#> ── Liiga games Information from liiga.fi ───────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-13 01:55:23 UTC
#> # A tibble: 60 × 59
#>    game_id season start      end   periods finished_type started ended game_time
#>      <int>  <int> <chr>      <chr> <chr>   <chr>         <lgl>   <lgl>     <int>
#>  1   55080   2026 2026-03-2… 2026… "[{\"i… ENDED_DURING… TRUE    TRUE       3600
#>  2   55081   2026 2026-03-2… 2026… "[{\"i… ENDED_DURING… TRUE    TRUE       3881
#>  3   55078   2026 2026-03-2… 2026… "[{\"i… ENDED_DURING… TRUE    TRUE       3600
#>  4   55079   2026 2026-03-2… 2026… "[{\"i… ENDED_DURING… TRUE    TRUE       3600
#>  5   55083   2026 2026-03-2… 2026… "[{\"i… ENDED_DURING… TRUE    TRUE       3600
#>  6   55082   2026 2026-03-2… 2026… "[{\"i… ENDED_DURING… TRUE    TRUE       4740
#>  7   55085   2026 2026-03-2… 2026… "[{\"i… ENDED_DURING… TRUE    TRUE       3600
#>  8   55084   2026 2026-03-2… 2026… "[{\"i… ENDED_DURING… TRUE    TRUE       3600
#>  9   55089   2026 2026-03-2… 2026… "[{\"i… ENDED_DURING… TRUE    TRUE       3600
#> 10   55088   2026 2026-03-2… 2026… "[{\"i… ENDED_DURING… TRUE    TRUE       4021
#> # ℹ 50 more rows
#> # ℹ 50 more variables: spectators <int>, play_off_pair <int>,
#> #   play_off_phase <int>, play_off_req_wins <int>, buy_tickets_url <chr>,
#> #   ice_rink_id <int>, ice_rink_name <chr>, ice_rink_latitude <dbl>,
#> #   ice_rink_longitude <dbl>, ice_rink_street_address <chr>,
#> #   ice_rink_zip <chr>, ice_rink_city <chr>, current_period <int>, stale <lgl>,
#> #   serie <chr>, game_week <int>, home_team_id <int>, home_team_slug <chr>, …
# }
```
