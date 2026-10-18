# **Ligue Magnus play-by-play**

Play-by-play from the game sheet: goals, penalties, shots, faceoffs,
goalie changes and timeouts in game order, with English event types and
derived periods. The sheet groups events by type and has no sequence
number, so events in the same second are ordered by a heuristic (shot,
goal, penalty, goalie change, timeout, faceoff). There are no shot
coordinates, and 2020-21 sheets carry no faceoff events. Every player
column is a person id; the on-ice lists are translated from per-game
participant ids through the roster.

## Usage

``` r
magnus_pbp(game_id, return_parsed = TRUE)
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

A tibble (`return_parsed = TRUE`; zero rows for an empty sheet), else
the game sheet list:

|  |  |  |
|----|----|----|
| col_name | type | description |
| game_id | integer | Game id (integer join key). |
| event_id | character | Event id as published, the raw type plus a number (e.g., "BUT99238"); unique within a game. |
| event_type | character | Event type in English: goal, penalty, shot, faceoff, goalie_change or timeout. |
| event_type_raw | character | Event type as published: BUT, SANCTION, TIR, ENGA, REMPLACANT or TEMPS_MORT. |
| period | integer | Period derived from the elapsed seconds (4 = overtime). A faceoff at 1200 / 2400 / 3600 opens the next period; any other event at a boundary belongs to the period ending there. |
| period_seconds | integer | Elapsed seconds within the period. |
| game_seconds | integer | Elapsed game seconds (the sheet's temps). |
| team_id | integer | Team id of the event's team (integer; for a faceoff, the winning team). |
| team_abbreviation | character | Abbreviation of the event's team. |
| player_id | integer | Person id (integer, stable across games) of the acting player; the scorer, the shooter, the penalised player or the faceoff winner. Null on goalie changes, timeouts and bench penalties. |
| player_name | character | Name of the acting player (SURNAME First). |
| assist1_player_id | integer | Person id of the first assist on a goal (integer). |
| assist1_player_name | character | Name of the first assist on a goal. |
| assist2_player_id | integer | Person id of the second assist on a goal (integer). |
| assist2_player_name | character | Name of the second assist on a goal. |
| goalie_player_id | integer | Person id of the goalie the goal or shot was against (integer). |
| goalie_player_name | character | Name of the goalie the goal or shot was against. |
| faceoff_home_player_id | integer | Person id of the home faceoff taker (integer). |
| faceoff_away_player_id | integer | Person id of the away faceoff taker (integer). |
| strength_code | character | Strength when a goal was scored: EG (even strength), SUP1 (power play, one man up); other codes unobserved. |
| penalty_code | character | Infraction code (e.g., CIN = slashing). |
| penalty | character | Infraction label in French (e.g., "Cinglage (coup de crosse)"). |
| penalty_minutes | integer | Penalty length in minutes. |
| penalty_start_seconds | integer | Elapsed game seconds when the penalty starts. |
| penalty_end_seconds | integer | Elapsed game seconds when the penalty ends. |
| goalie_in_player_id | integer | Person id of the goalie entering on a goalie change (integer; never observed populated). |
| goalie_out_player_id | integer | Person id of the goalie leaving on a goalie change (integer). |
| on_ice_plus_player_ids | list | Person ids of the scoring team's players on ice at a goal, goalie included (list of integer). |
| on_ice_minus_player_ids | list | Person ids of the conceding team's players on ice at a goal (list of integer). |

## Errors

As
[`magnus_competitions()`](https://fastRhockey.sportsdataverse.org/reference/magnus_competitions.md).

## See also

Other Ligue Magnus Functions:
[`magnus_competitions()`](https://fastRhockey.sportsdataverse.org/reference/magnus_competitions.md),
[`magnus_game()`](https://fastRhockey.sportsdataverse.org/reference/magnus_game.md),
[`magnus_lineups()`](https://fastRhockey.sportsdataverse.org/reference/magnus_lineups.md),
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
  try(magnus_pbp(game_id = 77779))
#> ── Ligue Magnus Play-by-play Information from liguemagnus.com ──────────────────
#> ℹ Data updated: 2026-10-18 06:15:30 UTC
#> # A tibble: 159 × 29
#>    game_id event_id event_type event_type_raw period period_seconds game_seconds
#>      <int> <chr>    <chr>      <chr>           <int>          <int>        <int>
#>  1   77779 ENGA237… faceoff    ENGA                1             60           60
#>  2   77779 ENGA237… faceoff    ENGA                1            253          253
#>  3   77779 SANCTIO… penalty    SANCTION            1            279          279
#>  4   77779 ENGA237… faceoff    ENGA                1            279          279
#>  5   77779 ENGA237… faceoff    ENGA                1            383          383
#>  6   77779 ENGA237… faceoff    ENGA                1            389          389
#>  7   77779 ENGA237… faceoff    ENGA                1            423          423
#>  8   77779 ENGA237… faceoff    ENGA                1            451          451
#>  9   77779 ENGA237… faceoff    ENGA                1            466          466
#> 10   77779 ENGA237… faceoff    ENGA                1            624          624
#> # ℹ 149 more rows
#> # ℹ 22 more variables: team_id <int>, team_abbreviation <chr>, player_id <int>,
#> #   player_name <chr>, assist1_player_id <int>, assist1_player_name <chr>,
#> #   assist2_player_id <int>, assist2_player_name <chr>, goalie_player_id <int>,
#> #   goalie_player_name <chr>, faceoff_home_player_id <int>,
#> #   faceoff_away_player_id <int>, strength_code <chr>, penalty_code <chr>,
#> #   penalty <chr>, penalty_minutes <int>, penalty_start_seconds <int>, …
# }
```
