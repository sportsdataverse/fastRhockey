# **Ligue Magnus standings**

Standings of a phase, one row per team in rank order. `wins` and
`losses` are regulation results; overtime and shootout results are
`ot_wins` and `ot_losses`.

## Usage

``` r
magnus_standings(phase_id, return_parsed = TRUE)
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
| rank | integer | Rank in the standings table. |
| team_id | integer | Team id (integer join key). |
| team | character | Short team name as the table publishes it (e.g., "MARSEILLE"). |
| team_abbreviation | character | Team abbreviation. |
| team_participant_id | integer | Team participant id in this phase (integer; joins home_team_participant_id / away_team_participant_id of the schedule). |
| games_played | integer | Games played in the table's scope. |
| wins | integer | Wins in regulation. |
| ot_wins | integer | Wins in overtime or a shootout. |
| ot_losses | integer | Losses in overtime or a shootout. |
| losses | integer | Losses in regulation. |
| ties | integer | Number of tied games. |
| points | integer | Points in the standings table. |
| goals_for | integer | Goals scored (total). |
| goals_against | integer | Goals allowed (goals against). |
| penalty_minutes | integer | Penalty minutes. |
| last_results | character | Form guide: the last results as W (win), L (loss) or T (tie) letters, in the order the table lists them. |
| standings_id | integer | Standings table id (integer). |

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
[`magnus_teams()`](https://fastRhockey.sportsdataverse.org/reference/magnus_teams.md)

## Examples

``` r
# \donttest{
  try(magnus_standings(phase_id = 714))
#> Error in .euro_check_status(resp) : 
#>   liguemagnus.com/wp-admin/admin-ajax.php answered HTTP 403:
#> <html><body><h1>403 Forbidden</h1> Request forbidden by administrative rules.
#> Interdiction d'acceder a cette ressource </body></html>
# }
```
