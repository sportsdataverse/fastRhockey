# **SIHF Team faceoffs per season**

Team faceoffs per season: taken, won, lost, per game, and win percentage
overall and by zone.

Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`,
keyless JSON). SIHF keys a season by its END year (2026 = 2025-26). An
unknown `season`, `league_id` or `phase_id` is never an error upstream
(SIHF silently answers another one), so every table answer is checked
against the body's own filter echo and refused when it differs. Every
request is spaced at least one second from the last one to the host.

## Usage

``` r
sihf_team_faceoffs(season, league_id = 1L, phase_id = NULL, language = "en")
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

A `fastRhockey_data` tibble with 18 columns. A zero-row tibble with
these columns when the payload holds no rows.

|  |  |  |
|----|----|----|
| col_name | type | description |
| season | integer | Season by its END year (2026 = 2025-26), from the body's echo of the Season filter. |
| phase_id | integer | SIHF phase id (regular season, a playoff round, ...); null unless a phase filter is passed. |
| rank | integer | Rank in the table; null on a summary row, and on a goaltender below the qualification line. |
| team_id | integer | SIHF team id (integer join key); null on a summary row (the pseudo-team 100000). |
| team_name | character | Team display name as SIHF publishes it; "Durchschnitt" on the league-average row, "Total" on the attendance total row. |
| team_acronym | character | Team abbreviation as SIHF publishes it (e.g., "HCD"); null on a summary row. |
| games_played | integer | Games played in the table's scope. |
| faceoffs | integer | Faceoffs taken, total. |
| faceoffs_per_game | numeric | Faceoffs taken per game. |
| faceoffs_percent | numeric | Faceoff win percentage; '-' with no faceoffs. |
| faceoffs_won | integer | Number of faceoffs won. |
| faceoffs_lost | integer | Number of faceoffs lost. |
| faceoffs_won_per_game | numeric | Faceoffs won per game. |
| faceoffs_lost_per_game | numeric | Faceoffs lost per game. |
| faceoffs_offensive_percent | numeric | Faceoff win percentage, offensive zone. |
| faceoffs_neutral_percent | numeric | Faceoff win percentage, neutral zone. |
| faceoffs_defensive_percent | numeric | Faceoff win percentage, defensive zone. |
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
  try(sihf_team_faceoffs(season = 2026, league_id = 1))
#> ── SIHF Team Faceoffs Information from data.sihf.ch ── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 12:12:15 UTC
#> # A tibble: 15 × 18
#>    season phase_id  rank team_id team_name    team_acronym games_played faceoffs
#>     <int>    <int> <int>   <int> <chr>        <chr>               <int>    <int>
#>  1   2026       NA    14  102126 SC Bern      SCB                    60     3126
#>  2   2026       NA    13  103141 Lausanne HC  LHC                    66     3651
#>  3   2026       NA    12  101152 HC Ambri-Pi… HCAP                   56     3144
#>  4   2026       NA    11  102128 EHC Biel-Bi… EHCB                   56     3028
#>  5   2026       NA    10  101150 HC Lugano    HCL                    60     3149
#>  6   2026       NA     9  101060 SC Rappersw… SCRJ                   74     4292
#>  7   2026       NA     8  101151 HC Davos     HCD                    86     4672
#>  8   2026       NA    NA      NA Durchschnitt NA                     65     3590
#>  9   2026       NA     7  101144 EV Zug       EVZ                    66     3483
#> 10   2026       NA     6  103144 HC Ajoie     HCA                    56     3019
#> 11   2026       NA     5  101139 ZSC Lions    ZSC                    70     3851
#> 12   2026       NA     4  103138 Fribourg-Go… FRI                    90     5002
#> 13   2026       NA     3  102127 SCL Tigers   SCL                    52     2904
#> 14   2026       NA     2  103140 Genève-Serv… GSHC                   76     4281
#> 15   2026       NA     1  101149 EHC Kloten   EHCK                   52     2674
#> # ℹ 10 more variables: faceoffs_per_game <dbl>, faceoffs_percent <dbl>,
#> #   faceoffs_won <int>, faceoffs_lost <int>, faceoffs_won_per_game <dbl>,
#> #   faceoffs_lost_per_game <dbl>, faceoffs_offensive_percent <dbl>,
#> #   faceoffs_neutral_percent <dbl>, faceoffs_defensive_percent <dbl>,
#> #   row_type <chr>
# }
```
