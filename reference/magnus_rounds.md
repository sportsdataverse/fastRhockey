# **Ligue Magnus rounds of a phase**

Rounds (journees) of a phase with their dates (games via
[`magnus_round_games()`](https://fastRhockey.sportsdataverse.org/reference/magnus_round_games.md)).
`round` is the value
[`magnus_round_games()`](https://fastRhockey.sportsdataverse.org/reference/magnus_round_games.md)
and
[`magnus_schedule()`](https://fastRhockey.sportsdataverse.org/reference/magnus_schedule.md)
take.

## Usage

``` r
magnus_rounds(phase_id, return_parsed = TRUE)
```

## Arguments

- phase_id:

  Phase id (`phase_id` of
  [`magnus_phases()`](https://fastRhockey.sportsdataverse.org/reference/magnus_phases.md)),
  e.g. 714.

- return_parsed:

  Parse the payload into a tibble (default `TRUE`). Pass `FALSE` for the
  raw JSON envelope as a list.

## Value

A tibble (`return_parsed = TRUE`), else the raw envelope list:

|  |  |  |
|----|----|----|
| col_name | type | description |
| round_id | integer | Round id (integer). |
| round | integer | Round (journée) number within the phase, the round argument of magnus_round_games and magnus_schedule. |
| round_label | character | Round label; null in every observed body. |
| phase_id | integer | Phase id (integer). |
| phase | character | Phase name in French. |
| competition_id | integer | Competition (season) id (integer). |
| season | integer | Season by its END year (2027 = 2026-27). |
| start_date | Date | First date of the round. |
| end_date | Date | Last date of the round. |

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
[`magnus_round_games()`](https://fastRhockey.sportsdataverse.org/reference/magnus_round_games.md),
[`magnus_schedule()`](https://fastRhockey.sportsdataverse.org/reference/magnus_schedule.md),
[`magnus_shootout()`](https://fastRhockey.sportsdataverse.org/reference/magnus_shootout.md),
[`magnus_standings()`](https://fastRhockey.sportsdataverse.org/reference/magnus_standings.md),
[`magnus_teams()`](https://fastRhockey.sportsdataverse.org/reference/magnus_teams.md)

## Examples

``` r
# \donttest{
  try(magnus_rounds(phase_id = 714))
#> ── Ligue Magnus Rounds Information from liguemagnus.com ────────────────────────
#> ℹ Data updated: 2026-10-18 05:48:59 UTC
#> # A tibble: 44 × 9
#>    round_id round round_label phase_id phase    competition_id season start_date
#>       <int> <int> <chr>          <int> <chr>             <int>  <int> <date>    
#>  1     9695     1 NA               714 Saison …            240   2027 2026-09-15
#>  2     9696     2 NA               714 Saison …            240   2027 2026-09-18
#>  3     9697     3 NA               714 Saison …            240   2027 2026-09-22
#>  4     9698     4 NA               714 Saison …            240   2027 2026-09-25
#>  5     9699     5 NA               714 Saison …            240   2027 2026-09-27
#>  6     9700     6 NA               714 Saison …            240   2027 2026-09-29
#>  7     9701     7 NA               714 Saison …            240   2027 2026-10-02
#>  8     9702     8 NA               714 Saison …            240   2027 2026-10-04
#>  9     9703     9 NA               714 Saison …            240   2027 2026-10-09
#> 10     9704    10 NA               714 Saison …            240   2027 2026-10-16
#> # ℹ 34 more rows
#> # ℹ 1 more variable: end_date <date>
# }
```
