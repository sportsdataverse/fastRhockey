# **Ligue Magnus shootout attempts**

Shootout attempts from the game sheet, in round order: `shootout_round`
is the round (one attempt per team per round), not a jersey number.
Trailing rounds that were not needed are listed with no shooter and
`scored` `NA`. Zero rows when the game had no shootout.

## Usage

``` r
magnus_shootout(game_id, return_parsed = TRUE)
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

A tibble (`return_parsed = TRUE`; zero rows without a shootout), else
the game sheet list:

|  |  |  |
|----|----|----|
| col_name | type | description |
| game_id | integer | Game id (integer join key). |
| attempt_id | integer | Shootout attempt id (integer). |
| shootout_round | integer | Shootout round of the attempt (one attempt per team per round); not a jersey number. |
| team_id | integer | Team id of the shooting team (integer). |
| team_abbreviation | character | Abbreviation of the shooting team. |
| shooter_player_id | integer | Person id of the shooter (integer); null on rounds not needed. |
| shooter_name | character | Name of the shooter. |
| goalie_player_id | integer | Person id of the goalie facing the attempt (integer). |
| goalie_name | character | Name of the goalie facing the attempt. |
| scored | logical | Whether the attempt scored; null on rounds not needed. |

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
[`magnus_standings()`](https://fastRhockey.sportsdataverse.org/reference/magnus_standings.md),
[`magnus_teams()`](https://fastRhockey.sportsdataverse.org/reference/magnus_teams.md)

## Examples

``` r
# \donttest{
  try(magnus_shootout(game_id = 77779))
#> Error in .euro_check_status(resp) : 
#>   liguemagnus.com/rencontre/77779/ answered HTTP 403: <html><body><h1>403
#> Forbidden</h1> Request forbidden by administrative rules. Interdiction
#> d'acceder a cette ressource </body></html>
# }
```
