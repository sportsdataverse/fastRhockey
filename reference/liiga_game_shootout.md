# **Liiga game shootout**

One game's shootout attempts: shooter, goalkeeper and result (none for a
game without one).

## Usage

``` r
liiga_game_shootout(season, game_id)
```

## Arguments

- season:

  Season END year (2026 = 2025-26). Game ids repeat across seasons, so a
  game is (season, game_id).

- game_id:

  Game id (game_id in liiga_schedule or liiga_games), e.g. 1; unique
  only within a season.

## Value

A `fastRhockey_data` tibble (11 columns; zero rows when a valid payload
has none).

|  |  |  |
|----|----|----|
| col_name | type | description |
| `season` | integer | Season END year (2026 = 2025-26). |
| `game_id` | integer | Game id, unique only within a season: key a game as (season, game_id). |
| `shooting_player_id` | integer | Shooter's player id. |
| `blocking_player_id` | integer | Goalkeeper's player id. |
| `shot_number` | integer | Attempt number in the shootout. |
| `shooting_team_id` | integer | Team id of the shooter's team (the number of the `<num>:<slug>` key). |
| `shooting_team_slug` | character | Slug of the shooter's team (e.g. hpk). |
| `blocking_team_id` | integer | Goalkeeper's team id. |
| `blocking_team_slug` | character | Goalkeeper's team slug. |
| `goal_scored` | logical | The attempt scored. |
| `winning_goal` | logical | Game-winning goal. |

## Details

Endpoint: `GET https://liiga.fi/api/v2/games/{season}/{game_id}`

Seasons are END years (2026 = 2025-26), the same key as
sportsdataverse-py. Requests to liiga.fi are spaced at least one second
apart. A request Liiga answers with nothing (an unknown game, a
tournament not played that season, a shot map before 2016) raises
`fastRhockey_no_data`; a failed fetch or an error-shaped answer raises
`fastRhockey_asset_fetch`; a bad argument raises
`fastRhockey_bad_request`.

## See also

[family_liiga](https://fastRhockey.sportsdataverse.org/reference/family_liiga.md)
lists every function in this family.

## Examples

``` r
# \donttest{
  try(liiga_game_shootout(game_id = 1, season = 2026))
#> ── Liiga game shootout Information from liiga.fi ────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 19:37:23 UTC
#> # A tibble: 6 × 11
#>   season game_id shooting_player_id blocking_player_id shot_number
#>    <int>   <int>              <int>              <int>       <int>
#> 1   2026       1           34320235           30117507           1
#> 2   2026       1           60900264           40107053           2
#> 3   2026       1           30851669           30117507           3
#> 4   2026       1           27023853           40107053           4
#> 5   2026       1           29885743           30117507           5
#> 6   2026       1           31569277           40107053           6
#> # ℹ 6 more variables: shooting_team_id <int>, shooting_team_slug <chr>,
#> #   blocking_team_id <int>, blocking_team_slug <chr>, goal_scored <lgl>,
#> #   winning_goal <lgl>
# }
```
