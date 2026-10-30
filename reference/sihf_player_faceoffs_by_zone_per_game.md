# **SIHF Skater faceoffs by zone (offensive, neutral, defensive) per game**

Skater faceoffs by zone (offensive, neutral, defensive) per game: taken,
won and lost.

Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`,
keyless JSON). SIHF keys a season by its END year (2026 = 2025-26). An
unknown `season`, `league_id` or `phase_id` is never an error upstream
(SIHF silently answers another one), so every table answer is checked
against the body's own filter echo and refused when it differs. Every
request is spaced at least one second from the last one to the host.

## Usage

``` r
sihf_player_faceoffs_by_zone_per_game(
  season,
  league_id = 1L,
  phase_id = NULL,
  language = "en"
)
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
  every phase.

- language:

  Response language. Only part of the body is translated: status,
  position and penalty names stay German.

## Value

A `fastRhockey_data` tibble with 19 columns. A zero-row tibble with
these columns when the payload holds no rows.

|  |  |  |
|----|----|----|
| col_name | type | description |
| season | integer | Season by its END year (2026 = 2025-26), from the body's echo of the Season filter. |
| phase_id | integer | SIHF phase id (regular season, a playoff round, ...); null unless a phase filter is passed. |
| rank | integer | Rank in the table; null on a summary row, and on a goaltender below the qualification line. |
| player | character | Player name as SIHF prints it, last name first (e.g., "Dahlbeck Klas"); season tables carry no player id. |
| team_id | integer | SIHF team id (integer join key); null on a summary row (the pseudo-team 100000). |
| team_name | character | Team display name as SIHF publishes it; "Durchschnitt" on the league-average row, "Total" on the attendance total row. |
| team_acronym | character | Team abbreviation as SIHF publishes it (e.g., "HCD"); null on a summary row. |
| position | character | Position, German whatever the language (e.g., "Stürmer"); null when SIHF prints "-". |
| games_played | integer | Games played in the table's scope. |
| faceoffs_offensive_per_game | numeric | Faceoffs taken per game, offensive zone. |
| faceoffs_won_offensive_per_game | numeric | Faceoffs won per game, offensive zone. |
| faceoffs_lost_offensive_per_game | numeric | Faceoffs lost per game, offensive zone. |
| faceoffs_neutral_per_game | numeric | Faceoffs taken per game, neutral zone. |
| faceoffs_won_neutral_per_game | numeric | Faceoffs won per game, neutral zone. |
| faceoffs_lost_neutral_per_game | numeric | Faceoffs lost per game, neutral zone. |
| faceoffs_defensive_per_game | numeric | Faceoffs taken per game, defensive zone. |
| faceoffs_won_defensive_per_game | numeric | Faceoffs won per game, defensive zone. |
| faceoffs_lost_defensive_per_game | numeric | Faceoffs lost per game, defensive zone. |
| row_type | character | "data" for a regular row; "average" (league average) or "total" (attendance total) for a summary row SIHF mixes into the table. |

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
  try(sihf_player_faceoffs_by_zone_per_game(season = 2026, league_id = 1))
#> ── SIHF Player Faceoffs By Zone Per Game Information from data.sihf.ch ─────────
#> ℹ Data updated: 2026-10-30 00:20:40 UTC
#> # A tibble: 427 × 19
#>    season phase_id  rank player          team_id team_name team_acronym position
#>     <int>    <int> <int> <chr>             <int> <chr>     <chr>        <chr>   
#>  1   2026       NA   427 Zadina Filip     101151 HC Davos  HCD          Stürmer 
#>  2   2026       NA   426 Rathgeb Yannick  103138 Fribourg… FRI          Verteid…
#>  3   2026       NA   425 Puljujärvi Jes…  103140 Genève-S… GSHC         Stürmer 
#>  4   2026       NA   424 Dahlbeck Klas    101151 HC Davos  HCD          Verteid…
#>  5   2026       NA   423 Rutta Jan        103140 Genève-S… GSHC         Verteid…
#>  6   2026       NA   422 Andersson Calle  101151 HC Davos  HCD          Verteid…
#>  7   2026       NA   421 Kapla Michael    103138 Fribourg… FRI          Verteid…
#>  8   2026       NA   420 Knak Simon       101151 HC Davos  HCD          Stürmer 
#>  9   2026       NA   419 Balcers Rudolfs  101139 ZSC Lions ZSC          Stürmer 
#> 10   2026       NA   418 Sörensen Marcus  103138 Fribourg… FRI          Stürmer 
#> # ℹ 417 more rows
#> # ℹ 11 more variables: games_played <int>, faceoffs_offensive_per_game <dbl>,
#> #   faceoffs_won_offensive_per_game <dbl>,
#> #   faceoffs_lost_offensive_per_game <dbl>, faceoffs_neutral_per_game <dbl>,
#> #   faceoffs_won_neutral_per_game <dbl>, faceoffs_lost_neutral_per_game <dbl>,
#> #   faceoffs_defensive_per_game <dbl>, faceoffs_won_defensive_per_game <dbl>,
#> #   faceoffs_lost_defensive_per_game <dbl>, row_type <chr>
# }
```
