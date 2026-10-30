# **Liiga game box score**

One game's per-period player box score, skaters and goalies, with
tracking where Liiga has it.

## Usage

``` r
liiga_game_stats(season, game_id)
```

## Arguments

- season:

  Season END year (2026 = 2025-26). Game ids repeat across seasons, so a
  game is (season, game_id).

- game_id:

  Game id (game_id in liiga_schedule or liiga_games), e.g. 1; unique
  only within a season.

## Value

A `fastRhockey_data` tibble (60 columns; zero rows when a valid payload
has none).

|  |  |  |
|----|----|----|
| col_name | type | description |
| `season` | integer | Season END year (2026 = 2025-26). |
| `game_id` | integer | Game id, unique only within a season: key a game as (season, game_id). |
| `side` | character | Which team of the game the row belongs to: home or away. |
| `team_id` | integer | Team id, the number of Liiga's `<num>:<slug>` team key (sent as that string, a bare integer or a digit string depending on the route). |
| `team_slug` | character | Team slug, the part of Liiga's `<num>:<slug>` team key after the colon (e.g. hpk). |
| `period` | integer | Period number: 1-3 are regulation, 4 and up overtime (or the shootout). |
| `player_id` | integer | Player id; the same number in lineups, rosters, stat tables and the profile (fihaId). |
| `jersey` | integer | Jersey (sweater) number the player wore. |
| `is_goalie` | logical | Row is a goalkeeper's (goalie columns filled). |
| `points` | integer | Points (player, this period). |
| `assists` | integer | Assists (player, this period). |
| `goals` | integer | Goals (player, this period). |
| `plusminus` | integer | plus minus minus, this period. |
| `plus` | integer | On ice for goals for (the + of plus/minus). |
| `minus` | integer | On ice for goals against (the - of plus/minus). |
| `shots` | integer | Shots (player, this period). |
| `penaltyminutes` | integer | Penaltyminutes (player, this period). |
| `timeofice` | integer | Timeofice (player, this period). Seconds. |
| `powerplay_goals` | integer | Powerplay goals (player, this period). |
| `short_handed_goals` | integer | Short handed goals (player, this period). |
| `winning_goal` | logical | Game-winning goal. |
| `blocked_shots` | integer | Blocked shots (player, this period). |
| `faceoffs_total` | integer | Faceoffs the player took in the period; null for every player in the 2026 captures. |
| `faceoffs_won` | integer | Faceoffs the player won in the period; null for every player in the 2026 captures. |
| `corsi_for` | integer | Corsi (shot attempts) for (player, this period). |
| `corsi_against` | integer | Corsi (shot attempts) against (player, this period). |
| `faceoffs_center_total` | integer | Faceoffs the player took at centre ice in the period; null for every player in the 2026 captures. |
| `faceoffs_center_won` | integer | Faceoffs the player won at centre ice in the period; null for every player in the 2026 captures. |
| `faceoffs_defence_total` | integer | Faceoffs the player took in the defensive zone in the period; null for every player in the 2026 captures. |
| `faceoffs_defence_won` | integer | Faceoffs the player won in the defensive zone in the period; null for every player in the 2026 captures. |
| `faceoffs_offence_total` | integer | Faceoffs the player took in the offensive zone in the period; null for every player in the 2026 captures. |
| `faceoffs_offence_won` | integer | Faceoffs the player won in the offensive zone in the period; null for every player in the 2026 captures. |
| `fs_zone_starts_oz` | integer | Full-strength (fs, unverified) zone starts offensive zone (player, this period). |
| `fs_zone_starts_dz` | integer | Full-strength (fs, unverified) zone starts defensive zone (player, this period). |
| `powerplay2_goals` | integer | Powerplay 2 goals (player, this period). |
| `penaltykill2_goals` | integer | Penaltykill 2 goals (player, this period). |
| `powerplay_assists` | integer | Powerplay assists (player, this period). |
| `penaltykill_assists` | integer | Penaltykill assists (player, this period). |
| `goals_to_empty_goal` | integer | Goals to empty goal (player, this period). |
| `fs_team_shots` | integer | Full-strength (fs, unverified) team shots (player, this period). |
| `fs_team_goals` | integer | Full-strength (fs, unverified) team goals (player, this period). |
| `fs_team_shots_against` | integer | Full-strength (fs, unverified) team shots against (player, this period). |
| `fs_team_goals_against` | integer | Full-strength (fs, unverified) team goals against (player, this period). |
| `distance` | numeric | Distance (player, this period). Distance; unit not stated (unverified). |
| `total_passes` | integer | Total passes (player, this period). |
| `successful_passes` | integer | Successful passes (player, this period). |
| `player_passes_under_pressure` | integer | Player passes under pressure (player, this period). |
| `player_successful_passes_under_pressure` | integer | Player successful passes under pressure (player, this period). |
| `player_passes_under_high_pressure` | integer | Player passes under high pressure (player, this period). |
| `player_successful_passes_under_high_pressure` | integer | Player successful passes under high pressure (player, this period). |
| `expected_goals_player` | numeric | Expected goals (xG) of the player's own shots in the period, provider-computed (unverified scope). |
| `expected_goals_team` | numeric | Team expected goals (xG) while the player was on ice in the period, provider-computed (unverified scope). |
| `expected_goals_against` | numeric | Expected goals against (xG) faced in the period, provider-computed. |
| `expected_goals_against_shot_on_goal` | numeric | Expected goals against (xG) of the shots on goal only, provider-computed. |
| `saves` | integer | Saves (goalkeeper, this period). |
| `goals_allowed` | integer | Goals allowed (goalkeeper, this period). |
| `saves_percentage` | numeric | Save percentage, 0-100, sent as a STRING (e.g. "92.86"; "" when no shots), this period. |
| `shots_on_goal` | integer | Shots on goal (goalkeeper, this period). |
| `top_speed` | numeric | Player's top skating speed in the game (unit not stated; values near 9 fit m/s). |
| `hardest_shot` | numeric | Player's hardest shot in the game (unit not stated; values near 23 fit m/s). |

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
  try(liiga_game_stats(game_id = 1, season = 2026))
#> ── Liiga game box score Information from liiga.fi ──── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-30 00:15:24 UTC
#> # A tibble: 210 × 60
#>    season game_id side   team_id team_slug period player_id jersey is_goalie
#>     <int>   <int> <chr>    <int> <chr>      <int>     <int>  <int> <lgl>    
#>  1   2026       1 home  55786244 hpk            1  21870028     64 FALSE    
#>  2   2026       1 home  55786244 hpk            1  22709389     74 FALSE    
#>  3   2026       1 home  55786244 hpk            1  23029644     19 FALSE    
#>  4   2026       1 home  55786244 hpk            1  23806498     63 FALSE    
#>  5   2026       1 home  55786244 hpk            1  29885743     93 FALSE    
#>  6   2026       1 home  55786244 hpk            1  30417647     15 FALSE    
#>  7   2026       1 home  55786244 hpk            1  30670073      6 FALSE    
#>  8   2026       1 home  55786244 hpk            1  30851669     25 FALSE    
#>  9   2026       1 home  55786244 hpk            1  31585390     43 FALSE    
#> 10   2026       1 home  55786244 hpk            1  32919839     50 FALSE    
#> # ℹ 200 more rows
#> # ℹ 51 more variables: points <int>, assists <int>, goals <int>,
#> #   plusminus <int>, plus <int>, minus <int>, shots <int>,
#> #   penaltyminutes <int>, timeofice <int>, powerplay_goals <int>,
#> #   short_handed_goals <int>, winning_goal <lgl>, blocked_shots <int>,
#> #   faceoffs_total <int>, faceoffs_won <int>, corsi_for <int>,
#> #   corsi_against <int>, faceoffs_center_total <int>, …
# }
```
