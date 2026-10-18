# **Ligue Magnus games of one round**

One round (journee) of a phase with its games (`get_rencontrestours`). A
round body publishes UTC ISO times and no win type: `game_datetime` is
converted to Europe/Paris and `decision` is `NA`.

## Usage

``` r
magnus_round_games(phase_id, round, return_parsed = TRUE)
```

## Arguments

- phase_id:

  Phase id (`phase_id` of
  [`magnus_phases()`](https://fastRhockey.sportsdataverse.org/reference/magnus_phases.md)),
  e.g. 714 (the 2026-27 regular season).

- round:

  Round number (tour), 1-based. Shadows the base function.

- return_parsed:

  Parse the payload into a tibble (default `TRUE`). Pass `FALSE` for the
  raw JSON envelope as a list.

## Value

A tibble (`return_parsed = TRUE`), else the raw envelope list:

|  |  |  |
|----|----|----|
| col_name | type | description |
| game_id | integer | Game id (integer join key), the game_id argument of magnus_game, magnus_pbp, magnus_lineups and magnus_shootout. |
| season | integer | Season by its END year (2027 = 2026-27). |
| competition_id | integer | Competition (season) id (integer). |
| phase_id | integer | Phase id (integer; regular season, playoffs, relegation). |
| phase | character | Phase name in French (e.g., "Saison régulière"). |
| round | integer | Round (journée) number within the phase. |
| game_datetime | POSIXct | Scheduled start, Europe/Paris time (get_rencontres publishes local time, a round body UTC; both are converted). |
| status | character | Game state: final, in_progress or scheduled. |
| decision | character | How a final game was decided: regulation, overtime or shootout; null until the game is final (round bodies carry no decision, so null there too). |
| home_team_id | integer | Team id of the home team (integer). |
| home_team | character | Display name of the home team. |
| home_team_abbreviation | character | Abbreviation of the home team. |
| away_team_id | integer | Team id of the away team (integer). |
| away_team | character | Display name of the away team. |
| away_team_abbreviation | character | Abbreviation of the away team. |
| home_score | integer | Goals of the home team (shootout winner included in the final score); null until the game starts (unplayed games publish a 0-0 placeholder). |
| away_score | integer | Goals of the away team; null until the game starts. |
| home_team_participant_id | integer | Team participant id of the home team in this phase (integer; its own key space, joins team_participant_id of the standings; not a team id). |
| away_team_participant_id | integer | Team participant id of the away team in this phase (integer; joins team_participant_id of the standings). |
| venue_id | integer | Rink id (integer). |
| venue | character | Name of the rink (arena). |
| game_label | character | Game label as published ("HOME / AWAY"). |

## Errors

As
[`magnus_competitions()`](https://fastRhockey.sportsdataverse.org/reference/magnus_competitions.md).

## See also

Other Ligue Magnus Functions:
[`magnus_competitions()`](https://fastRhockey.sportsdataverse.org/reference/magnus_competitions.md),
[`magnus_game()`](https://fastRhockey.sportsdataverse.org/reference/magnus_game.md),
[`magnus_lineups()`](https://fastRhockey.sportsdataverse.org/reference/magnus_lineups.md),
[`magnus_pbp()`](https://fastRhockey.sportsdataverse.org/reference/magnus_pbp.md),
[`magnus_phases()`](https://fastRhockey.sportsdataverse.org/reference/magnus_phases.md),
[`magnus_rounds()`](https://fastRhockey.sportsdataverse.org/reference/magnus_rounds.md),
[`magnus_schedule()`](https://fastRhockey.sportsdataverse.org/reference/magnus_schedule.md),
[`magnus_shootout()`](https://fastRhockey.sportsdataverse.org/reference/magnus_shootout.md),
[`magnus_standings()`](https://fastRhockey.sportsdataverse.org/reference/magnus_standings.md),
[`magnus_teams()`](https://fastRhockey.sportsdataverse.org/reference/magnus_teams.md)

## Examples

``` r
# \donttest{
  try(magnus_round_games(phase_id = 714, round = 1))
#> ── Ligue Magnus Round Games Information from liguemagnus.com ───────────────────
#> ℹ Data updated: 2026-10-18 05:48:58 UTC
#> # A tibble: 6 × 22
#>   game_id season competition_id phase_id phase  round game_datetime       status
#>     <int>  <int>          <int>    <int> <chr>  <int> <dttm>              <chr> 
#> 1   77745   2027            240      714 Saiso…     1 2026-09-15 20:00:00 final 
#> 2   77746   2027            240      714 Saiso…     1 2026-09-15 20:00:00 final 
#> 3   77744   2027            240      714 Saiso…     1 2026-09-15 20:15:00 final 
#> 4   77749   2027            240      714 Saiso…     1 2026-09-15 20:15:00 final 
#> 5   77747   2027            240      714 Saiso…     1 2026-09-15 20:30:00 final 
#> 6   77748   2027            240      714 Saiso…     1 2026-09-15 20:30:00 final 
#> # ℹ 14 more variables: decision <chr>, home_team_id <int>, home_team <chr>,
#> #   home_team_abbreviation <chr>, away_team_id <int>, away_team <chr>,
#> #   away_team_abbreviation <chr>, home_score <int>, away_score <int>,
#> #   home_team_participant_id <int>, away_team_participant_id <int>,
#> #   venue_id <int>, venue <chr>, game_label <chr>
# }
```
