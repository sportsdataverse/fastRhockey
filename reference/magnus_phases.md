# **Ligue Magnus phases of a season**

Phases of a competition (regular season, playoffs, relegation), with the
`phase_id` the phase-scoped routes take.

## Usage

``` r
magnus_phases(season, return_parsed = TRUE)
```

## Arguments

- season:

  Season by its END year (2027 = 2026-27), sent as the season's
  `competition_id`. Coverage starts in 2020-21 (2021); an earlier
  season, or one the federation does not list, is `fastRhockey_no_data`.

- return_parsed:

  Parse the payload into a tibble (default `TRUE`). Pass `FALSE` for the
  raw JSON envelope as a list.

## Value

A tibble (`return_parsed = TRUE`), else the raw envelope list:

|  |  |  |
|----|----|----|
| col_name | type | description |
| phase_id | integer | Phase id (integer), the value the phase_id argument of magnus_teams, magnus_rounds, magnus_round_games, magnus_standings and magnus_schedule takes. Ids change every season. |
| phase | character | Phase name in French (e.g., "Saison régulière" = regular season). |
| phase_type_code | character | Phase type code: C (championnat, round robin) or T (tableau, playoff bracket). |
| visible | logical | Whether the phase is shown on the public site. |
| competition | character | Name of the competition (season) the phase belongs to. |
| n_teams | integer | Number of teams in the phase. |
| overtime | logical | Whether tied games go to overtime in the phase. |
| games_per_matchup | integer | Games each pair of teams plays in the phase. |

## Errors

As
[`magnus_competitions()`](https://fastRhockey.sportsdataverse.org/reference/magnus_competitions.md).

## See also

Other Ligue Magnus Functions:
[`magnus_competitions()`](https://fastRhockey.sportsdataverse.org/reference/magnus_competitions.md),
[`magnus_game()`](https://fastRhockey.sportsdataverse.org/reference/magnus_game.md),
[`magnus_lineups()`](https://fastRhockey.sportsdataverse.org/reference/magnus_lineups.md),
[`magnus_pbp()`](https://fastRhockey.sportsdataverse.org/reference/magnus_pbp.md),
[`magnus_round_games()`](https://fastRhockey.sportsdataverse.org/reference/magnus_round_games.md),
[`magnus_rounds()`](https://fastRhockey.sportsdataverse.org/reference/magnus_rounds.md),
[`magnus_schedule()`](https://fastRhockey.sportsdataverse.org/reference/magnus_schedule.md),
[`magnus_shootout()`](https://fastRhockey.sportsdataverse.org/reference/magnus_shootout.md),
[`magnus_standings()`](https://fastRhockey.sportsdataverse.org/reference/magnus_standings.md),
[`magnus_teams()`](https://fastRhockey.sportsdataverse.org/reference/magnus_teams.md)

## Examples

``` r
# \donttest{
  try(magnus_phases(season = 2027))
#> ── Ligue Magnus Phases Information from liguemagnus.com ────────────────────────
#> ℹ Data updated: 2026-10-13 01:56:49 UTC
#> # A tibble: 1 × 8
#>   phase_id phase            phase_type_code visible competition n_teams overtime
#>      <int> <chr>            <chr>           <lgl>   <chr>         <int> <lgl>   
#> 1      714 Saison régulière C               TRUE    Synerglace…      12 TRUE    
#> # ℹ 1 more variable: games_per_matchup <int>
# }
```
