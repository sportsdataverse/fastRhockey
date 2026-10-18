# **Ligue Magnus teams of a phase**

Teams of a phase. `team_id` is stable across seasons and joins games and
standings; `competition_ids` lists every competition the team entered
(all seasons and levels).

## Usage

``` r
magnus_teams(phase_id, return_parsed = TRUE)
```

## Arguments

- phase_id:

  Phase id (`phase_id` of
  [`magnus_phases()`](https://fastRhockey.sportsdataverse.org/reference/magnus_phases.md)),
  e.g. 714. Required: without it the API answers every team of the
  federation (~680 KB).

- return_parsed:

  Parse the payload into a tibble (default `TRUE`). Pass `FALSE` for the
  raw JSON envelope as a list.

## Value

A tibble (`return_parsed = TRUE`), else the raw envelope list:

|  |  |  |
|----|----|----|
| col_name | type | description |
| team_id | integer | Team id (integer join key), stable across seasons; joins home_team_id / away_team_id of the schedule and team_id of the standings. |
| team | character | Team display name (e.g., "Rapaces de Gap"). |
| team_abbreviation | character | Team abbreviation (e.g., "GAP"). |
| team_short_name | character | Short team label (e.g., "GAP", "ANGLET"). |
| team_label | character | Back-office label, " - "; use team for display. |
| gender | character | Gender of the team: M (men; the only value observed). |
| logo_url | character | URL of the team logo image. |
| club_id | integer | Id of the club (legal entity) behind the team (integer). |
| club | character | Name of the club (legal entity). |
| club_code | character | Federation club code (e.g., "93006S"). |
| competition_ids | list | Competition ids the team has entered, all seasons and levels (list of integer). |

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
[`magnus_rounds()`](https://fastRhockey.sportsdataverse.org/reference/magnus_rounds.md),
[`magnus_schedule()`](https://fastRhockey.sportsdataverse.org/reference/magnus_schedule.md),
[`magnus_shootout()`](https://fastRhockey.sportsdataverse.org/reference/magnus_shootout.md),
[`magnus_standings()`](https://fastRhockey.sportsdataverse.org/reference/magnus_standings.md)

## Examples

``` r
# \donttest{
  try(magnus_teams(phase_id = 714))
#> ── Ligue Magnus Teams Information from liguemagnus.com ─────────────────────────
#> ℹ Data updated: 2026-10-18 06:15:37 UTC
#> # A tibble: 12 × 11
#>    team_id team     team_abbreviation team_short_name team_label gender logo_url
#>      <int> <chr>    <chr>             <chr>           <chr>      <chr>  <chr>   
#>  1      75 Anglet … HOR               ANGLET          72001S - … M      https:/…
#>  2     103 Gothiqu… AMI               AMIENS          22001S - … M      https:/…
#>  3     253 Dragons… ROU               ROUEN           23001S - … M      https:/…
#>  4     327 Brûleur… GRE               GRENOBLE        82007S - … M      https:/…
#>  5     345 Jokers … CER               CERGY-PONTOISE  11013S - … M      https:/…
#>  6     366 Rapaces… GAP               GAP             93006S - … M      https:/…
#>  7     472 Aigles … NIC               NICE            93008S - … M      https:/…
#>  8     553 Ducs d'… ANG               ANGERS          52007S - … M      https:/…
#>  9     611 Pionnie… CHA               CHAMONIX        82005S - … M      https:/…
#> 10    1671 BOXERS … BOR               BORDEAUX        72003S - … M      https:/…
#> 11    3095 Spartia… MAR               MARSEILLE       93019S - … M      https:/…
#> 12    3409 DIABLES… BRI               BRIANÇON        93003S - … M      https:/…
#> # ℹ 4 more variables: club_id <int>, club <chr>, club_code <chr>,
#> #   competition_ids <list>
# }
```
