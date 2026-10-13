# **SIHF Skater box score of one game, both sides, with ice time and faceoffs**

Skater box score of one game, both sides, with ice time and faceoffs.

Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`,
keyless JSON). SIHF keys a season by its END year (2026 = 2025-26). An
unknown `season`, `league_id` or `phase_id` is never an error upstream
(SIHF silently answers another one), so every table answer is checked
against the body's own filter echo and refused when it differs. Every
request is spaced at least one second from the last one to the host.

## Usage

``` r
sihf_game_player_stats(game_id, language = "en")
```

## Arguments

- game_id:

  Required. SIHF game id (game_id in sihf_results), e.g. 20261105000325.

- language:

  Response language. Only part of the body is translated: status,
  position and penalty names stay German.

## Value

A `fastRhockey_data` tibble with 26 columns. A zero-row tibble with
these columns when the payload holds no rows.

|  |  |  |
|----|----|----|
| col_name | type | description |
| game_id | numeric | SIHF game id (numeric join key; the details link of sihf_results, the searchQuery of the game wrappers). |
| team_side | character | Side of the game the row belongs to: "home" or "away". |
| team_id | integer | SIHF team id of the row's side. |
| team_name | character | Team name of the row's side. |
| player_number | integer | Jersey number worn in this game. |
| player | character | Player name as SIHF prints it, last name first (e.g., "Dahlbeck Klas"). |
| player_position | character | Position, German whatever the language (e.g., "Verteidiger"). |
| goals | integer | Goals scored by the player. |
| assists | integer | Assists (primary + secondary). |
| assists1 | integer | Primary assists. |
| assists2 | integer | Secondary assists. |
| points | integer | Points (goals + assists). |
| pim_total | integer | Penalty minutes. |
| plus_minus | integer | Plus-minus: even-strength and shorthanded goals for, minus against, while on ice. |
| shots_on_goal | integer | Shots on goal taken by the player. |
| sog_missed | integer | Shots that missed the net. |
| sog_post | integer | Shots off the post or crossbar. |
| blockings | integer | Opponent shots blocked by the player. |
| faceoffs_won | integer | Faceoffs won by the player. |
| faceoffs_lost | integer | Faceoffs lost by the player. |
| faceoffs_percent | numeric | Faceoff win percentage, 0-100; null with no faceoffs. |
| time_on_ice | character | Ice time, "mm:ss". |
| time_on_ice_pp | character | Power-play ice time, "mm:ss". |
| time_on_ice_pk | character | Penalty-kill ice time, "mm:ss". |
| row_type | character | "data" per player, "total" for the side's total row. |
| player_id | integer | SIHF licence number of the player (integer join key), resolved from the game's roster. |

## Errors

- `fastRhockey_no_data`: SIHF answered 404 (unknown game), or ignored a
  requested season, league or phase and served another one.

- `fastRhockey_bad_request`: SIHF answered 400 / 422, or an argument is
  invalid.

- `fastRhockey_asset_fetch`: any other non-2xx answer or a dead
  connection after retries, an empty or unreadable body, a stub or
  redirect body, a table without its filter echo, a truncated table, or
  a `gameoverview` body for another game.

## See also

Other SIHF Functions:
[`sihf_game_goalie_stats()`](https://fastRhockey.sportsdataverse.org/reference/sihf_game_goalie_stats.md),
[`sihf_game_info()`](https://fastRhockey.sportsdataverse.org/reference/sihf_game_info.md),
[`sihf_game_lineups()`](https://fastRhockey.sportsdataverse.org/reference/sihf_game_lineups.md),
[`sihf_game_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/sihf_game_team_stats.md),
[`sihf_goalie_shootouts()`](https://fastRhockey.sportsdataverse.org/reference/sihf_goalie_shootouts.md),
[`sihf_goalie_shots()`](https://fastRhockey.sportsdataverse.org/reference/sihf_goalie_shots.md),
[`sihf_goalie_stats()`](https://fastRhockey.sportsdataverse.org/reference/sihf_goalie_stats.md),
[`sihf_goalie_wins()`](https://fastRhockey.sportsdataverse.org/reference/sihf_goalie_wins.md),
[`sihf_pbp()`](https://fastRhockey.sportsdataverse.org/reference/sihf_pbp.md),
[`sihf_phases()`](https://fastRhockey.sportsdataverse.org/reference/sihf_phases.md),
[`sihf_player_faceoffs()`](https://fastRhockey.sportsdataverse.org/reference/sihf_player_faceoffs.md),
[`sihf_player_faceoffs_by_zone()`](https://fastRhockey.sportsdataverse.org/reference/sihf_player_faceoffs_by_zone.md),
[`sihf_player_faceoffs_by_zone_per_game()`](https://fastRhockey.sportsdataverse.org/reference/sihf_player_faceoffs_by_zone_per_game.md),
[`sihf_player_goals_assists()`](https://fastRhockey.sportsdataverse.org/reference/sihf_player_goals_assists.md),
[`sihf_player_penalties()`](https://fastRhockey.sportsdataverse.org/reference/sihf_player_penalties.md),
[`sihf_player_shootouts()`](https://fastRhockey.sportsdataverse.org/reference/sihf_player_shootouts.md),
[`sihf_player_shots()`](https://fastRhockey.sportsdataverse.org/reference/sihf_player_shots.md),
[`sihf_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/sihf_player_stats.md),
[`sihf_player_time_on_ice()`](https://fastRhockey.sportsdataverse.org/reference/sihf_player_time_on_ice.md),
[`sihf_results()`](https://fastRhockey.sportsdataverse.org/reference/sihf_results.md),
[`sihf_standings()`](https://fastRhockey.sportsdataverse.org/reference/sihf_standings.md),
[`sihf_team_attendance()`](https://fastRhockey.sportsdataverse.org/reference/sihf_team_attendance.md),
[`sihf_team_faceoffs()`](https://fastRhockey.sportsdataverse.org/reference/sihf_team_faceoffs.md),
[`sihf_team_faceoffs_by_zone()`](https://fastRhockey.sportsdataverse.org/reference/sihf_team_faceoffs_by_zone.md),
[`sihf_team_faceoffs_by_zone_per_game()`](https://fastRhockey.sportsdataverse.org/reference/sihf_team_faceoffs_by_zone_per_game.md),
[`sihf_team_goals()`](https://fastRhockey.sportsdataverse.org/reference/sihf_team_goals.md),
[`sihf_team_goals_by_position()`](https://fastRhockey.sportsdataverse.org/reference/sihf_team_goals_by_position.md),
[`sihf_team_penalties()`](https://fastRhockey.sportsdataverse.org/reference/sihf_team_penalties.md),
[`sihf_team_penalty_kill()`](https://fastRhockey.sportsdataverse.org/reference/sihf_team_penalty_kill.md),
[`sihf_team_power_play()`](https://fastRhockey.sportsdataverse.org/reference/sihf_team_power_play.md),
[`sihf_team_shootouts()`](https://fastRhockey.sportsdataverse.org/reference/sihf_team_shootouts.md),
[`sihf_team_shots()`](https://fastRhockey.sportsdataverse.org/reference/sihf_team_shots.md)

## Examples

``` r
# \donttest{
  try(sihf_game_player_stats(game_id = 20261105000325))
#> ── SIHF Game Player Stats Information from data.sihf.ch ────────────────────────
#> ℹ Data updated: 2026-10-13 01:59:38 UTC
#> # A tibble: 42 × 26
#>    game_id team_side team_id team_name player_number player      player_position
#>      <dbl> <chr>       <int> <chr>             <int> <chr>       <chr>          
#>  1 2.03e13 home       101151 HC Davos              6 Dahlbeck K… Verteidiger    
#>  2 2.03e13 home       101151 HC Davos              8 Gredig Rico Stürmer        
#>  3 2.03e13 home       101151 HC Davos             15 Ryfors Sim… Stürmer        
#>  4 2.03e13 home       101151 HC Davos             18 Zadina Fil… Stürmer        
#>  5 2.03e13 home       101151 HC Davos             20 Fora Micha… Verteidiger    
#>  6 2.03e13 home       101151 HC Davos             21 Parrée Jul… Stürmer        
#>  7 2.03e13 home       101151 HC Davos             22 Lemieux Br… Stürmer        
#>  8 2.03e13 home       101151 HC Davos             25 Waidacher … Stürmer        
#>  9 2.03e13 home       101151 HC Davos             36 Knak Simon  Stürmer        
#> 10 2.03e13 home       101151 HC Davos             38 Frick Lukas Verteidiger    
#> # ℹ 32 more rows
#> # ℹ 19 more variables: goals <int>, assists <int>, assists1 <int>,
#> #   assists2 <int>, points <int>, pim_total <int>, plus_minus <int>,
#> #   shots_on_goal <int>, sog_missed <int>, sog_post <int>, blockings <int>,
#> #   faceoffs_won <int>, faceoffs_lost <int>, faceoffs_percent <dbl>,
#> #   time_on_ice <chr>, time_on_ice_pp <chr>, time_on_ice_pk <chr>,
#> #   row_type <chr>, player_id <int>
# }
```
