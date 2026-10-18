# **Liiga team stat tables**

Team stat table of a season range, by data_type.

## Usage

``` r
liiga_team_stats(
  data_type,
  season_from = NULL,
  season_to = NULL,
  tournament = "runkosarja"
)
```

## Arguments

- data_type:

  Stat table: shots, passes, faceoffs, penalties, powerplay,
  penalty_kill, even_strength.

- season_from:

  First season (END year) of the range.

- season_to:

  Last season (END year) of the range.

- tournament:

  Tournament: runkosarja (regular season), playoffs, playout,
  qualifications, valmistavat_ottelut (preseason) or chl (Champions
  Hockey League). Required by the API; defaults to runkosarja.

## Value

A `fastRhockey_data` tibble (67 columns; zero rows when a valid payload
has none).

|  |  |  |
|----|----|----|
| col_name | type | description |
| `sort_desc` | logical | Default sort is descending. |
| `team_id` | integer | Team id, the number of Liiga's `<num>:<slug>` team key (sent as that string, a bare integer or a digit string depending on the route). |
| `team_name` | character | Team's display name, e.g. HPK or Tappara. |
| `team_short_name` | character | Team abbreviation. |
| `team_logos_dark_bg` | character | Logo image URL for dark backgrounds. |
| `team_logos_light_bg` | character | Logo image URL for light backgrounds. |
| `team_logos_dark_bg_original` | character | Original-file logo URL for dark backgrounds; null in every capture. |
| `team_logos_light_bg_original` | character | Original-file logo URL for light backgrounds; null in every capture. |
| `games` | integer | Games (team, over the season range). |
| `ranking` | integer | Table position. |
| `previous_game_day_ranking` | integer | Table position after the previous game day. |
| `shots` | integer | Shots (team, over the season range). |
| `shots_saved` | integer | Shots saved (team, over the season range). |
| `shots_even_strength` | integer | Shots at even strength (team, over the season range). |
| `shots_powerplay` | integer | Shots on the power play (team, over the season range). |
| `shots_penalty_kill` | integer | Shots on the penalty kill (team, over the season range). |
| `shots_against` | integer | Shots against (team, over the season range). |
| `goals` | integer | Goals (team, over the season range). |
| `goals_against` | integer | Goals against (team, over the season range). |
| `shots_against_even_strength` | integer | Shots against at even strength (team, over the season range). |
| `shots_against_powerplay` | integer | Shots against on the power play (team, over the season range). |
| `shots_against_penalty_kill` | integer | Shots against on the penalty kill (team, over the season range). |
| `non_blocked_shots` | integer | Non blocked shots (team, over the season range). |
| `non_blocked_shots_even_strength` | integer | Non blocked shots at even strength (team, over the season range). |
| `non_blocked_shots_powerplay` | integer | Non blocked shots on the power play (team, over the season range). |
| `non_blocked_shots_penalty_kill` | integer | Non blocked shots on the penalty kill (team, over the season range). |
| `non_blocked_shots_against` | integer | Non blocked shots against (team, over the season range). |
| `non_blocked_shots_against_even_strength` | integer | Non blocked shots against at even strength (team, over the season range). |
| `non_blocked_shots_against_powerplay` | integer | Non blocked shots against on the power play (team, over the season range). |
| `non_blocked_shots_against_penalty_kill` | integer | Non blocked shots against on the penalty kill (team, over the season range). |
| `powerplay_goals` | integer | Powerplay goals (team, over the season range). |
| `powerplay_goals_against` | integer | Powerplay goals against (team, over the season range). |
| `penalty_kill_goals` | integer | Penalty kill goals (team, over the season range). |
| `penalty_kill_goals_against` | integer | Penalty kill goals against (team, over the season range). |
| `even_strength_goals` | integer | Even strength goals (team, over the season range). |
| `even_strength_goals_against` | integer | Even strength goals against (team, over the season range). |
| `previous_game_day_goals` | integer | Goals as of the previous game day (team, over the season range). |
| `previous_game_day_shots` | integer | Shots as of the previous game day (team, over the season range). |
| `previous_game_day_shots_percentage` | numeric | Shots percentage as of the previous game day (team, over the season range). Percent, 0-100. |
| `shots_percentage_even_strength` | numeric | Shots percentage at even strength (team, over the season range). Percent, 0-100. |
| `shots_percentage_powerplay` | numeric | Shots percentage on the power play (team, over the season range). Percent, 0-100. |
| `shots_percentage_penalty_kill` | numeric | Shots percentage on the penalty kill (team, over the season range). Percent, 0-100. |
| `shots_difference_even_strength` | integer | Shots difference at even strength (team, over the season range). |
| `shots_difference_powerplay` | integer | Shots difference on the power play (team, over the season range). |
| `shots_difference_penalty_kill` | integer | Shots difference on the penalty kill (team, over the season range). |
| `shots_for_shots_against_percentage` | numeric | Shots for shots against percentage (team, over the season range). Percent, 0-100. |
| `non_blocked_shots_difference` | integer | Non blocked shots difference (team, over the season range). |
| `save_percentage_even_strength` | numeric | Save percentage at even strength (team, over the season range). Percent, 0-100. |
| `save_percentage_powerplay` | numeric | Save percentage on the power play (team, over the season range). Percent, 0-100. |
| `save_percentage_penalty_kill` | numeric | Save percentage on the penalty kill (team, over the season range). Percent, 0-100. |
| `shots_difference` | integer | Shots difference (team, over the season range). |
| `pdo_penalty_kill` | numeric | PDO (on-ice shooting % + save %) on the penalty kill (team, over the season range). Percent, 0-100. |
| `pdo_even_strength` | numeric | PDO (on-ice shooting % + save %) at even strength (team, over the season range). Percent, 0-100. |
| `pdo_powerplay` | numeric | PDO (on-ice shooting % + save %) on the power play (team, over the season range). Percent, 0-100. |
| `shots_percentage` | numeric | Shots percentage (team, over the season range). Percent, 0-100. |
| `non_blocked_shots_for_shots_against_percentage_even_strength` | numeric | Non blocked shots for shots against percentage at even strength (team, over the season range). Percent, 0-100. |
| `non_blocked_shots_for_shots_against_percentage_powerplay` | numeric | Non blocked shots for shots against percentage on the power play (team, over the season range). Percent, 0-100. |
| `non_blocked_shots_for_shots_against_percentage_penalty_kill` | numeric | Non blocked shots for shots against percentage on the penalty kill (team, over the season range). Percent, 0-100. |
| `shots_for_shots_against_percentage_even_strength` | numeric | Shots for shots against percentage at even strength (team, over the season range). Percent, 0-100. |
| `shots_for_shots_against_percentage_powerplay` | numeric | Shots for shots against percentage on the power play (team, over the season range). Percent, 0-100. |
| `shots_for_shots_against_percentage_penalty_kill` | numeric | Shots for shots against percentage on the penalty kill (team, over the season range). Percent, 0-100. |
| `non_blocked_shots_difference_powerplay` | integer | Non blocked shots difference on the power play (team, over the season range). |
| `non_blocked_shots_difference_penalty_kill` | integer | Non blocked shots difference on the penalty kill (team, over the season range). |
| `non_blocked_shots_for_shots_against_percentage` | numeric | Non blocked shots for shots against percentage (team, over the season range). Percent, 0-100. |
| `pdo` | numeric | PDO (on-ice shooting % + save %) (team, over the season range). Percent, 0-100. |
| `non_blocked_shots_difference_even_strength` | integer | Non blocked shots difference at even strength (team, over the season range). |
| `save_percentage` | numeric | Save percentage (team, over the season range). Percent, 0-100. |

## Details

Endpoint: `GET https://liiga.fi/api/v2/teams/stats`

Seasons are END years (2026 = 2025-26), the same key as sdv-py. Requests
to liiga.fi are spaced at least one second apart. A request Liiga
answers with nothing (an unknown game, a tournament not played that
season, a shot map before 2016) raises `fastRhockey_no_data`; a failed
fetch or an error-shaped answer raises `fastRhockey_asset_fetch`; a bad
argument raises `fastRhockey_bad_request`.

The columns below are those of the default `data_type`; the other tables
carry their own stat columns next to `team_id` / `player_id`.

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
[`liiga_teams()`](https://fastRhockey.sportsdataverse.org/reference/liiga_teams.md),
[`liiga_teams_history()`](https://fastRhockey.sportsdataverse.org/reference/liiga_teams_history.md),
[`liiga_tournament()`](https://fastRhockey.sportsdataverse.org/reference/liiga_tournament.md)

## Examples

``` r
# \donttest{
  try(liiga_team_stats(data_type = "shots", season_from = 2026, season_to = 2026))
#> ── Liiga team stat tables Information from liiga.fi ── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 12:07:02 UTC
#> # A tibble: 16 × 67
#>    sort_desc    team_id team_name team_short_name team_logos_dark_bg            
#>    <lgl>          <int> <chr>     <chr>           <chr>                         
#>  1 FALSE      626537494 Sport     SPO             https://cdn.builder.io/api/v1…
#>  2 FALSE      651304385 TPS       TPS             https://cdn.builder.io/api/v1…
#>  3 FALSE      168761288 HIFK      IFK             https://cdn.builder.io/api/v1…
#>  4 FALSE       55786244 HPK       HPK             https://cdn.builder.io/api/v1…
#>  5 FALSE     1368624751 K-Espoo   KES             https://cdn.builder.io/api/v1…
#>  6 FALSE      875886777 Pelicans  PEL             https://cdn.builder.io/api/v1…
#>  7 FALSE      951626834 Ilves     ILV             https://cdn.builder.io/api/v1…
#>  8 FALSE      624554857 Lukko     LUK             https://cdn.builder.io/api/v1…
#>  9 FALSE      292293444 Jukurit   JUK             https://cdn.builder.io/api/v1…
#> 10 FALSE      679171680 Ässät     ÄSS             https://cdn.builder.io/api/v1…
#> 11 FALSE      495643563 Kärpät    KÄR             https://cdn.builder.io/api/v1…
#> 12 FALSE      933686567 SaiPa     SAI             https://cdn.builder.io/api/v1…
#> 13 FALSE      219244634 JYP       JYP             https://cdn.builder.io/api/v1…
#> 14 FALSE      461765763 KooKoo    KOO             https://cdn.builder.io/api/v1…
#> 15 FALSE      362185137 Tappara   TAP             https://cdn.builder.io/api/v1…
#> 16 FALSE      859884935 KalPa     KAL             https://cdn.builder.io/api/v1…
#> # ℹ 62 more variables: team_logos_light_bg <chr>,
#> #   team_logos_dark_bg_original <chr>, team_logos_light_bg_original <chr>,
#> #   games <int>, ranking <int>, previous_game_day_ranking <int>, shots <int>,
#> #   shots_saved <int>, shots_even_strength <int>, shots_powerplay <int>,
#> #   shots_penalty_kill <int>, shots_against <int>, goals <int>,
#> #   goals_against <int>, shots_against_even_strength <int>,
#> #   shots_against_powerplay <int>, shots_against_penalty_kill <int>, …
# }
```
