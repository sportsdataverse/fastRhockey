# **Ligue Magnus game header**

Game header from the game sheet (`/rencontre/{game_id}/`): teams, score,
start time, status, attendance and rink (one row). Staff birth dates are
dropped from the sheet before it is returned.

## Usage

``` r
magnus_game(game_id, return_parsed = TRUE)
```

## Arguments

- game_id:

  Game id (`game_id` of
  [`magnus_schedule()`](https://fastRhockey.sportsdataverse.org/reference/magnus_schedule.md)),
  e.g. 77779 (2026-27, shootout and MVPs); also sampled: 68838, 865.

- return_parsed:

  Parse the payload into a tibble (default `TRUE`). Pass `FALSE` for the
  game sheet extracted from the page's embedded JSON, as a list.

## Value

A tibble (`return_parsed = TRUE`; one row), else the game sheet list:

|  |  |  |
|----|----|----|
| col_name | type | description |
| game_id | integer | Game id (integer join key). |
| competition | character | Competition name as published. |
| phase | character | Phase name in French. |
| round | integer | Round (journée) number. |
| game_datetime | POSIXct | Start time, Europe/Paris. |
| status | character | Game state: final, in_progress or scheduled. |
| home_team_id | integer | Team id of the home team (integer). |
| home_team | character | Display name of the home team. |
| home_team_abbreviation | character | Abbreviation of the home team. |
| away_team_id | integer | Team id of the away team (integer). |
| away_team | character | Display name of the away team. |
| away_team_abbreviation | character | Abbreviation of the away team. |
| home_score | integer | Goals of the home team (the final score, shootout included). |
| away_score | integer | Goals of the away team (the final score, shootout included). |
| home_team_participant_id | integer | Team participant id of the home team in this phase (integer; joins the standings). |
| away_team_participant_id | integer | Team participant id of the away team in this phase (integer; joins the standings). |
| has_shootout | logical | Whether the game went to a shootout. |
| n_periods | integer | Number of regulation periods. |
| period_minutes | integer | Length of a regulation period in minutes. |
| attendance | integer | Number of spectators (attendance). |
| venue | character | Name of the rink (arena). |

## Errors

As
[`magnus_competitions()`](https://fastRhockey.sportsdataverse.org/reference/magnus_competitions.md).

## See also

Other Ligue Magnus Functions:
[`magnus_competitions()`](https://fastRhockey.sportsdataverse.org/reference/magnus_competitions.md),
[`magnus_lineups()`](https://fastRhockey.sportsdataverse.org/reference/magnus_lineups.md),
[`magnus_pbp()`](https://fastRhockey.sportsdataverse.org/reference/magnus_pbp.md),
[`magnus_phases()`](https://fastRhockey.sportsdataverse.org/reference/magnus_phases.md),
[`magnus_round_games()`](https://fastRhockey.sportsdataverse.org/reference/magnus_round_games.md),
[`magnus_rounds()`](https://fastRhockey.sportsdataverse.org/reference/magnus_rounds.md),
[`magnus_schedule()`](https://fastRhockey.sportsdataverse.org/reference/magnus_schedule.md),
[`magnus_shootout()`](https://fastRhockey.sportsdataverse.org/reference/magnus_shootout.md),
[`magnus_standings()`](https://fastRhockey.sportsdataverse.org/reference/magnus_standings.md),
[`magnus_teams()`](https://fastRhockey.sportsdataverse.org/reference/magnus_teams.md)

## Examples

``` r
# \donttest{
  try(magnus_game(game_id = 77779))
#> ── Ligue Magnus Game Information from liguemagnus.com ──────────────────────────
#> ℹ Data updated: 2026-10-18 12:08:32 UTC
#> # A tibble: 1 × 21
#>   game_id competition        phase round game_datetime       status home_team_id
#>     <int> <chr>              <chr> <int> <dttm>              <chr>         <int>
#> 1   77779 Synerglace Ligue … Sais…     6 2026-09-29 20:30:00 final           553
#> # ℹ 14 more variables: home_team <chr>, home_team_abbreviation <chr>,
#> #   away_team_id <int>, away_team <chr>, away_team_abbreviation <chr>,
#> #   home_score <int>, away_score <int>, home_team_participant_id <int>,
#> #   away_team_participant_id <int>, has_shootout <lgl>, n_periods <int>,
#> #   period_minutes <int>, attendance <int>, venue <chr>
# }
```
