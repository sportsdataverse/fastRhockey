# **SIHF Games of a league season**

Games of a league season: date, teams, score, per-period score,
decision, status and game_id.

Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`,
keyless JSON). SIHF keys a season by its END year (2026 = 2025-26). An
unknown `season`, `league_id` or `phase_id` is never an error upstream
(SIHF silently answers another one), so every table answer is checked
against the body's own filter echo and refused when it differs. Every
request is spaced at least one second from the last one to the host.

## Usage

``` r
sihf_results(season, league_id = 1L, phase_id = NULL, language = "en")
```

## Arguments

- season:

  Required. Season by its END year (2026 = 2025-26). A season SIHF does
  not have raises `fastRhockey_no_data` error (the API itself silently
  answers another one).

- league_id:

  SIHF league id: 1 = National League, 2 = Swiss League, 100 = MyHockey
  League, 3 = 1. Liga, 43 = SWHL B, 113 = U21-Elit (the body's League
  filter lists them). An id SIHF does not have for the season raises
  `fastRhockey_no_data` error.

- phase_id:

  SIHF phase id (regular season, a playoff round, ...; the body's Phase
  filter lists them). `NULL` = no phase filter: the season summary over
  every phase (player / goalie tables), every game (results), the
  regular-season table (standings).

- language:

  Response language. Only part of the body is translated: status,
  position and penalty names stay German.

## Value

A `fastRhockey_data` tibble with 26 columns. A zero-row tibble with
these columns when the payload holds no rows.

|  |  |  |
|----|----|----|
| col_name | type | description |
| season | integer | Season by its END year (2026 = 2025-26), from the body's echo of the Season filter. |
| phase_id | integer | SIHF phase id (regular season, a playoff round, ...); null unless a phase filter is passed. |
| day | character | Weekday abbreviation, German (e.g., "Di"). |
| date | Date | Game date (local). |
| time | character | Scheduled local start time, "hh:mm". |
| home_team_id | integer | SIHF team id of the home team. |
| home_team_name | character | Home team display name. |
| home_team_acronym | character | Home team abbreviation. |
| away_team_id | integer | SIHF team id of the away team. |
| away_team_name | character | Away team display name. |
| away_team_acronym | character | Away team abbreviation. |
| score_home | integer | Final home score. |
| score_away | integer | Final away score. |
| score_per_period_home | list | Home goals per period (overtime included as its own period). |
| score_per_period_away | list | Away goals per period (overtime included as its own period). |
| decision | character | How the game was decided beyond regulation: "OT" or "SO"; null in regulation. |
| status_id | integer | SIHF game-status id (12 = finished). |
| status_name | character | Game-status name; German whatever the language (e.g., "Ende"). |
| status_percent | integer | Game progress, 0-100 (100 once the game is over). |
| status_canceled | logical | Whether the game was cancelled. |
| start_date_time | character | Scheduled start, ISO-8601 with UTC offset, as a string. |
| end_date_time | character | End of the game, ISO-8601 with UTC offset, as a string. |
| update_date_time | character | When SIHF last updated the record, ISO-8601 with UTC offset, as a string. |
| game_id | numeric | SIHF game id (numeric join key; the details link of sihf_results, the searchQuery of the game wrappers). |
| broadcasts | character | Broadcasters, comma-joined (e.g., "MYSPORTS,TV24"). |
| row_type | character | "data" for a regular row; "average" or "total" for a summary row SIHF mixes into the table. |

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
[`sihf_game_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/sihf_game_player_stats.md),
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
  try(sihf_results(season = 2026, league_id = 1))
#> ── SIHF Results Information from data.sihf.ch ──────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-30 00:20:47 UTC
#> # A tibble: 414 × 26
#>    season phase_id day   date       time  home_team_id home_team_name           
#>     <int>    <int> <chr> <date>     <chr>        <int> <chr>                    
#>  1   2026       NA Di    2025-09-09 19:45       101152 HC Ambri-Piotta          
#>  2   2026       NA Di    2025-09-09 19:45       101151 HC Davos                 
#>  3   2026       NA Di    2025-09-09 19:45       103138 Fribourg-Gottéron        
#>  4   2026       NA Di    2025-09-09 19:45       103140 Genève-Servette HC       
#>  5   2026       NA Di    2025-09-09 19:45       101060 SC Rapperswil-Jona Lakers
#>  6   2026       NA Di    2025-09-09 19:45       101139 ZSC Lions                
#>  7   2026       NA Di    2025-09-09 19:45       101144 EV Zug                   
#>  8   2026       NA Do    2025-09-11 20:00       102128 EHC Biel-Bienne          
#>  9   2026       NA Fr    2025-09-12 19:45       103144 HC Ajoie                 
#> 10   2026       NA Fr    2025-09-12 19:45       102126 SC Bern                  
#> # ℹ 404 more rows
#> # ℹ 19 more variables: home_team_acronym <chr>, away_team_id <int>,
#> #   away_team_name <chr>, away_team_acronym <chr>, score_home <int>,
#> #   score_away <int>, score_per_period_home <list>,
#> #   score_per_period_away <list>, decision <chr>, status_id <int>,
#> #   status_name <chr>, status_percent <int>, status_canceled <lgl>,
#> #   start_date_time <chr>, end_date_time <chr>, update_date_time <chr>, …
# }
```
