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
#> ── Ligue Magnus Standings Information from liguemagnus.com ─────────────────────
#> ℹ Data updated: 2026-10-30 00:17:16 UTC
#> # A tibble: 12 × 17
#>     rank team_id team   team_abbreviation team_participant_id games_played  wins
#>    <int>   <int> <chr>  <chr>                           <int>        <int> <int>
#>  1     1     327 GRENO… GRE                              9125           12    10
#>  2     2     253 ROUEN  ROU                              9127           14     9
#>  3     3    3095 MARSE… MAR                              9129           14     7
#>  4     4     611 CHAMO… CHA                              9123           12     6
#>  5     5     553 ANGERS ANG                              9130           13     6
#>  6     6     472 NICE   NIC                              9126           13     5
#>  7     7    1671 BORDE… BOR                              9121           13     5
#>  8     8     103 AMIENS AMI                              9120           13     4
#>  9     9     345 CERGY… CER                              9122           13     4
#> 10    10     366 GAP    GAP                              9124           13     3
#> 11    11    3409 BRIAN… BRI                              9131           13     3
#> 12    12      75 ANGLET HOR                              9128           13     1
#> # ℹ 10 more variables: ot_wins <int>, ot_losses <int>, losses <int>,
#> #   ties <int>, points <int>, goals_for <int>, goals_against <int>,
#> #   penalty_minutes <int>, last_results <chr>, standings_id <int>
# }
```
