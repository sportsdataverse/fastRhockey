# **SIHF Lineups of one game by unit and slot, plus the absentees**

Lineups of one game by unit and slot, plus the absentees.

Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`,
keyless JSON). SIHF keys a season by its END year (2026 = 2025-26). An
unknown `season`, `league_id` or `phase_id` is never an error upstream
(SIHF silently answers another one), so every table answer is checked
against the body's own filter echo and refused when it differs. Every
request is spaced at least one second from the last one to the host.

## Usage

``` r
sihf_game_lineups(game_id, language = "en")
```

## Arguments

- game_id:

  Required. SIHF game id (game_id in sihf_results), e.g. 20261105000325.

- language:

  Response language. Only part of the body is translated: status,
  position and penalty names stay German.

## Value

A `fastRhockey_data` tibble with 12 columns. A zero-row tibble with
these columns when the payload holds no rows.

|  |  |  |
|----|----|----|
| col_name | type | description |
| game_id | numeric | SIHF game id (numeric join key; the details link of sihf_results, the searchQuery of the game wrappers). |
| team_side | character | Side of the game the row belongs to: "home" or "away". |
| team_id | integer | SIHF team id (integer join key). |
| unit | character | Lineup unit: "goalkeeper", "defense_left" / "\_right", "forward_left" / "\_center" / "\_right", "other"; null for an absentee. |
| slot | integer | 1-based position in SIHF's listing of the unit. |
| player_id | integer | SIHF licence number of the player (integer join key), resolved from the game's roster. |
| player_name | character | Player name from the game's roster, last name first. |
| player_jersey | integer | Jersey number from the game's roster. |
| is_captain | logical | Whether the lineup tags the player as captain. |
| is_first_goalkeeper | logical | Whether the lineup tags the player as the starting goaltender. |
| absence_code | character | Absence code for an absentee ("SU", "IN", "AB"). |
| absence_reason | character | Absence reason, German (e.g., "Überzählig", "Verletzt", "Abwesend"). |

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
  try(sihf_game_lineups(game_id = 20261105000325))
#> ── SIHF Game Lineups Information from data.sihf.ch ──────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 17:24:06 UTC
#> # A tibble: 49 × 12
#>    game_id team_side team_id unit       slot player_id player_name player_jersey
#>      <dbl> <chr>       <int> <chr>     <int>     <int> <chr>               <int>
#>  1 2.03e13 home       101151 goalkeep…     1    150983 Aeschliman…            29
#>  2 2.03e13 home       101151 goalkeep…     2    309536 Hollenstei…            51
#>  3 2.03e13 home       101151 defense_…     1    340162 Dahlbeck K…             6
#>  4 2.03e13 home       101151 defense_…     2    152416 Jung Sven              90
#>  5 2.03e13 home       101151 defense_…     3    312287 Barandun D…            57
#>  6 2.03e13 home       101151 defense_…     4    308175 Gross Nico             77
#>  7 2.03e13 home       101151 defense_…     1    153736 Frick Lukas            38
#>  8 2.03e13 home       101151 defense_…     2    322236 Andersson …            55
#>  9 2.03e13 home       101151 defense_…     3    153207 Fora Micha…            20
#> 10 2.03e13 home       101151 forward_…     1    346405 Zadina Fil…            18
#> # ℹ 39 more rows
#> # ℹ 4 more variables: is_captain <lgl>, is_first_goalkeeper <lgl>,
#> #   absence_code <chr>, absence_reason <chr>
# }
```
