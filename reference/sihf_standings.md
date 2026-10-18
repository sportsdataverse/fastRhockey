# **SIHF League table of a season (every game, or home / away games only)**

League table of a season (every game, or home / away games only): rank,
games, points, regulation / overtime / shootout wins and losses, goals,
streak.

Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`,
keyless JSON). SIHF keys a season by its END year (2026 = 2025-26). An
unknown `season`, `league_id` or `phase_id` is never an error upstream
(SIHF silently answers another one), so every table answer is checked
against the body's own filter echo and refused when it differs. Every
request is spaced at least one second from the last one to the host.

## Usage

``` r
sihf_standings(
  season,
  league_id = 1L,
  phase_id = NULL,
  venue = "All",
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
  filter lists them). `NULL` = no phase filter: the regular-season
  table.

- venue:

  Which games the table counts: All (the full table), Home or Away.
  Always sent: without it SIHF answers the away-games table (26 of the
  52 National League games).

- language:

  Response language. Only part of the body is translated: status,
  position and penalty names stay German.

## Value

A `fastRhockey_data` tibble with 20 columns. A zero-row tibble with
these columns when the payload holds no rows.

|  |  |  |
|----|----|----|
| col_name | type | description |
| season | integer | Season by its END year (2026 = 2025-26), from the body's echo of the Season filter. |
| phase_id | integer | SIHF phase id (regular season, a playoff round, ...); null unless a phase filter is passed. |
| rank | integer | Rank in the table; null on a summary row. |
| team_id | integer | SIHF team id (integer join key). |
| team_name | character | Team display name as SIHF publishes it. |
| team_acronym | character | Team abbreviation as SIHF publishes it (e.g., "HCD"). |
| games_played | integer | Games the team played in the selection (every game, or home / away games only, per venue). |
| points | integer | Table points earned in the standings. |
| points_quotient | numeric | Points per game. |
| games_won_in_time | integer | Wins in regulation. |
| games_won_extra_time | integer | Wins in overtime. |
| games_won_penalty | integer | Wins in a shootout. |
| games_lost_penalty | integer | Losses in a shootout. |
| games_lost_extra_time | integer | Losses in overtime. |
| games_lost_in_time | integer | Losses in regulation. |
| goals_for | integer | Goals scored by the team. |
| goals_against | integer | Goals allowed by the team. |
| goals_difference | integer | Goal difference. |
| streak | character | Recent results, newest first, as SIHF prints them (e.g., "W+L-"; "+" regulation, "-" after regulation). |
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
[`sihf_results()`](https://fastRhockey.sportsdataverse.org/reference/sihf_results.md),
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
  try(sihf_standings(season = 2026, league_id = 1))
#> ── SIHF Standings Information from data.sihf.ch ────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 05:52:14 UTC
#> # A tibble: 14 × 20
#>    season phase_id  rank team_id team_name      team_acronym games_played points
#>     <int>    <int> <int>   <int> <chr>          <chr>               <int>  <int>
#>  1   2026       NA     1  101151 HC Davos       HCD                    52    117
#>  2   2026       NA     2  103138 Fribourg-Gott… FRI                    52    100
#>  3   2026       NA     3  103140 Genève-Servet… GSHC                   52     91
#>  4   2026       NA     4  101139 ZSC Lions      ZSC                    52     91
#>  5   2026       NA     5  101150 HC Lugano      HCL                    52     89
#>  6   2026       NA     6  103141 Lausanne HC    LHC                    52     85
#>  7   2026       NA     7  101060 SC Rapperswil… SCRJ                   52     81
#>  8   2026       NA     8  101144 EV Zug         EVZ                    52     75
#>  9   2026       NA     9  102126 SC Bern        SCB                    52     68
#> 10   2026       NA    10  102128 EHC Biel-Bien… EHCB                   52     67
#> 11   2026       NA    11  102127 SCL Tigers     SCL                    52     64
#> 12   2026       NA    12  101149 EHC Kloten     EHCK                   52     63
#> 13   2026       NA    13  101152 HC Ambri-Piot… HCAP                   52     59
#> 14   2026       NA    14  103144 HC Ajoie       HCA                    52     42
#> # ℹ 12 more variables: points_quotient <dbl>, games_won_in_time <int>,
#> #   games_won_extra_time <int>, games_won_penalty <int>,
#> #   games_lost_penalty <int>, games_lost_extra_time <int>,
#> #   games_lost_in_time <int>, goals_for <int>, goals_against <int>,
#> #   goals_difference <int>, streak <chr>, row_type <chr>
# }
```
