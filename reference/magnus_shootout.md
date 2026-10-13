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
#> ── Ligue Magnus Shootout Information from liguemagnus.com ──────────────────────
#> ℹ Data updated: 2026-10-13 01:56:52 UTC
#> # A tibble: 10 × 10
#>    game_id attempt_id shootout_round team_id team_abbreviation shooter_player_id
#>      <int>      <int>          <int>   <int> <chr>                         <int>
#>  1   77779      33605              1     553 ANG                           39651
#>  2   77779      33606              1    3409 BRI                          143117
#>  3   77779      33607              2     553 ANG                           99599
#>  4   77779      33608              2    3409 BRI                           54923
#>  5   77779      33609              3     553 ANG                          120435
#>  6   77779      33610              3    3409 BRI                          142947
#>  7   77779      33611              4     553 ANG                          120499
#>  8   77779      33612              4    3409 BRI                           69022
#>  9   77779      33613              5     553 ANG                              NA
#> 10   77779      33614              5    3409 BRI                              NA
#> # ℹ 4 more variables: shooter_name <chr>, goalie_player_id <int>,
#> #   goalie_name <chr>, scored <lgl>
# }
```
