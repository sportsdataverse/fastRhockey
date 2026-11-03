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
| team_label | character | Back-office label, `"<club code> - <NAME> <level>"`; use team for display. |
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
#> Error in .euro_check_status(resp) : 
#>   liguemagnus.com/wp-admin/admin-ajax.php answered HTTP 403:
#> <html><body><h1>403 Forbidden</h1> Request forbidden by administrative rules.
#> Interdiction d'acceder a cette ressource </body></html>
# }
```
