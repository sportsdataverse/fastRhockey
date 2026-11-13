# **SIHF Goaltender box score of one game, both sides**

Goaltender box score of one game, both sides.

Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`,
keyless JSON). SIHF keys a season by its END year (2026 = 2025-26). An
unknown `season`, `league_id` or `phase_id` is never an error upstream
(SIHF silently answers another one), so every table answer is checked
against the body's own filter echo and refused when it differs. Every
request is spaced at least one second from the last one to the host.

## Usage

``` r
sihf_game_goalie_stats(game_id, language = "en")
```

## Arguments

- game_id:

  Required. SIHF game id (game_id in sihf_results), e.g. 20261105000325.

- language:

  Response language. Only part of the body is translated: status,
  position and penalty names stay German.

## Value

A `fastRhockey_data` tibble with 15 columns. A zero-row tibble with
these columns when the payload holds no rows.

|  |  |  |
|----|----|----|
| col_name | type | description |
| game_id | numeric | SIHF game id (numeric join key; the details link of sihf_results, the searchQuery of the game wrappers). |
| team_side | character | Side of the game the row belongs to: "home" or "away". |
| team_id | integer | SIHF team id of the row's side. |
| team_name | character | Team name of the row's side. |
| player_number | integer | Jersey number worn in this game. |
| player | character | Player name as SIHF prints it, last name first (e.g., "Dahlbeck Klas"). |
| goals_against | integer | Goals allowed by the goaltender. |
| saves | integer | Shots on goal saved by the goaltender. |
| shots_against | integer | Shots on goal faced. |
| saves_percentage | numeric | Save percentage on a 0-100 scale (91.25 = .9125). |
| seconds_played | character | Ice time as "mm:ss" (a string, despite the upstream name). Minutes are not capped at 59: an overtime game reads "65:00", a season total "3615:02". |
| goals_against_average | numeric | Goals against per 60 minutes in this game. |
| penalty_in_minutes | integer | Penalty minutes. |
| row_type | character | "data" for every row: a game goalie box has no summary rows. |
| player_id | integer | SIHF licence number of the player (integer join key), resolved from the game's roster. |

## Errors

- `fastRhockey_no_data`: SIHF answered 404 (unknown game), or ignored a
  requested season, league or phase and served another one.

- `fastRhockey_bad_request`: SIHF answered 400 / 422, or an argument is
  invalid.

- `fastRhockey_asset_fetch`: any other non-2xx answer or a dead
  connection after retries, an empty or unreadable body, a stub or
  redirect body, a table without its filter echo, a truncated table, or
  a `gameoverview` body for another game.

## See also

[family_sihf](https://fastRhockey.sportsdataverse.org/reference/family_sihf.md)
lists every function in this family.

## Examples

``` r
# \donttest{
  try(sihf_game_goalie_stats(game_id = 20261105000325))
#> ── SIHF Game Goalie Stats Information from data.sihf.ch ─── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 20:23:20 UTC
#> # A tibble: 4 × 15
#>   game_id team_side team_id team_name  player_number player  goals_against saves
#>     <dbl> <chr>       <int> <chr>              <int> <chr>           <int> <int>
#> 1 2.03e13 home       101151 HC Davos              29 Aeschl…             3    19
#> 2 2.03e13 home       101151 HC Davos              51 Hollen…             0     0
#> 3 2.03e13 away       102127 SCL Tigers            39 Boltsh…             2    18
#> 4 2.03e13 away       102127 SCL Tigers            91 Meyer …             2    14
#> # ℹ 7 more variables: shots_against <int>, saves_percentage <dbl>,
#> #   seconds_played <chr>, goals_against_average <dbl>,
#> #   penalty_in_minutes <int>, row_type <chr>, player_id <int>
# }
```
