# **Ligue Magnus schedule and results**

Games (schedule and results) of a season from `get_rencontres`,
requested as one page (`par_page = 700`) that must be the whole list: a
partial page is `fastRhockey_asset_fetch`, never returned as a whole
schedule. `get_rencontres` publishes local times; a round body publishes
UTC: both land in `game_datetime` as Europe/Paris. Scores are matched to
the home and away teams by team id (`score[]` is not home-first and some
finished games list extra 0-goal rows for teams that did not play);
goals are `NA` until the game starts (unplayed games publish a 0-0
placeholder).

## Usage

``` r
magnus_schedule(
  season,
  phase_id = NULL,
  team_id = NULL,
  start_date = NULL,
  end_date = NULL,
  round = NULL,
  return_parsed = TRUE
)
```

## Arguments

- season:

  Season by its END year (2027 = 2026-27), sent as the season's
  `competition_id`. Coverage starts in 2020-21 (2021); an earlier
  season, or one the federation does not list, is `fastRhockey_no_data`.

- phase_id:

  Phase id (`phase_id` of
  [`magnus_phases()`](https://fastRhockey.sportsdataverse.org/reference/magnus_phases.md)).
  Omitted: every phase of the competition.

- team_id:

  Team id (`team_id` of
  [`magnus_teams()`](https://fastRhockey.sportsdataverse.org/reference/magnus_teams.md)):
  that team's games only.

- start_date:

  Earliest game date, `YYYY-MM-DD` (a string or a `Date`).

- end_date:

  Latest game date, `YYYY-MM-DD` (a string or a `Date`).

- round:

  Round number (tour): that round's games only. Shadows the base
  function.

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
[`magnus_round_games()`](https://fastRhockey.sportsdataverse.org/reference/magnus_round_games.md),
[`magnus_rounds()`](https://fastRhockey.sportsdataverse.org/reference/magnus_rounds.md),
[`magnus_shootout()`](https://fastRhockey.sportsdataverse.org/reference/magnus_shootout.md),
[`magnus_standings()`](https://fastRhockey.sportsdataverse.org/reference/magnus_standings.md),
[`magnus_teams()`](https://fastRhockey.sportsdataverse.org/reference/magnus_teams.md)

## Examples

``` r
# \donttest{
  try(magnus_schedule(season = 2027, phase_id = 714, round = 2))
#> ── Ligue Magnus Schedule Information from liguemagnus.com ──────────────────────
#> ℹ Data updated: 2026-10-18 12:08:38 UTC
#> # A tibble: 6 × 22
#>   game_id season competition_id phase_id phase  round game_datetime       status
#>     <int>  <int>          <int>    <int> <chr>  <int> <dttm>              <chr> 
#> 1   77753   2027            240      714 Saiso…     2 2026-09-18 20:00:00 final 
#> 2   77752   2027            240      714 Saiso…     2 2026-09-18 20:05:00 final 
#> 3   77751   2027            240      714 Saiso…     2 2026-09-18 20:15:00 final 
#> 4   77755   2027            240      714 Saiso…     2 2026-09-18 20:15:00 final 
#> 5   77750   2027            240      714 Saiso…     2 2026-09-18 20:30:00 final 
#> 6   77754   2027            240      714 Saiso…     2 2026-09-18 20:30:00 final 
#> # ℹ 14 more variables: decision <chr>, home_team_id <int>, home_team <chr>,
#> #   home_team_abbreviation <chr>, away_team_id <int>, away_team <chr>,
#> #   away_team_abbreviation <chr>, home_score <int>, away_score <int>,
#> #   home_team_participant_id <int>, away_team_participant_id <int>,
#> #   venue_id <int>, venue <chr>, game_label <chr>
# }
```
