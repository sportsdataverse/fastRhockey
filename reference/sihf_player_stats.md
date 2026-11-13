# **SIHF Skater season totals**

Skater season totals: games, goals, assists, points, penalty minutes,
plus-minus.

Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`,
keyless JSON). SIHF keys a season by its END year (2026 = 2025-26). An
unknown `season`, `league_id` or `phase_id` is never an error upstream
(SIHF silently answers another one), so every table answer is checked
against the body's own filter echo and refused when it differs. Every
request is spaced at least one second from the last one to the host.

## Usage

``` r
sihf_player_stats(season, league_id = 1L, phase_id = NULL, language = "en")
```

## Arguments

- season:

  Required. Season by its END year (2026 = 2025-26). A season SIHF does
  not have raises `fastRhockey_no_data` error (the API itself silently
  answers another one).

- league_id:

  SIHF league id: 1 = National League, 2 = Swiss League, 100 = MyHockey
  League, 3 = 1. Liga, 43 = SWHL B, 113 = U21-Elit (the body's League
  filter lists them). An id SIHF does not have for the season raises
  `fastRhockey_no_data` error.

- phase_id:

  SIHF phase id (regular season, a playoff round, ...; the body's Phase
  filter lists them). `NULL` = no phase filter: the season summary over
  every phase (player / goalie tables), every game (results), the
  regular-season table (standings).

- language:

  Response language. Only part of the body is translated: status,
  position and penalty names stay German.

## Value

A `fastRhockey_data` tibble with 16 columns. A zero-row tibble with
these columns when the payload holds no rows.

|  |  |  |
|----|----|----|
| col_name | type | description |
| season | integer | Season by its END year (2026 = 2025-26), from the body's echo of the Season filter. |
| phase_id | integer | SIHF phase id (regular season, a playoff round, ...); null unless a phase filter is passed. |
| rank | integer | Rank in the table; null on a summary row. |
| player | character | Player name as SIHF prints it, last name first (e.g., "Dahlbeck Klas"). |
| team_id | integer | SIHF team id (integer join key). |
| team_name | character | Team display name as SIHF publishes it. |
| team_acronym | character | Team abbreviation as SIHF publishes it (e.g., "HCD"). |
| position | character | Position, German whatever the language (e.g., "Stürmer"). |
| games_played | integer | Games the player played in the selection. |
| goals | integer | Goals scored by the player. |
| assists | integer | Assists (primary + secondary). |
| points | integer | Points (goals + assists). |
| points_per_game | numeric | Points per game. |
| pim_total | integer | Penalty minutes. |
| plus_minus | integer | Plus-minus: even-strength and shorthanded goals for, minus against, while on ice. |
| row_type | character | "data" for a regular row; "average" or "total" for a summary row SIHF mixes into the table. |

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
  try(sihf_player_stats(season = 2026, league_id = 1))
#> ── SIHF Player Stats Information from data.sihf.ch ──────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 21:35:08 UTC
#> # A tibble: 427 × 16
#>    season phase_id  rank player          team_id team_name team_acronym position
#>     <int>    <int> <int> <chr>             <int> <chr>     <chr>        <chr>   
#>  1   2026       NA   427 Paschoud Claud…  102127 SCL Tige… SCL          Verteid…
#>  2   2026       NA   426 Pouilly Bastien  103144 HC Ajoie  HCA          Verteid…
#>  3   2026       NA   425 Rod Julien       103138 Fribourg… FRI          Stürmer 
#>  4   2026       NA   424 Hornecker Jan    101060 SC Rappe… SCRJ         Stürmer 
#>  5   2026       NA   423 Schmutz Reto     103144 HC Ajoie  HCA          Stürmer 
#>  6   2026       NA   422 Olsson Daniel    101139 ZSC Lions ZSC          Stürmer 
#>  7   2026       NA   421 Schläpfer Elvis  103144 HC Ajoie  HCA          Stürmer 
#>  8   2026       NA   420 Fahrni Joshua    102127 SCL Tige… SCL          Stürmer 
#>  9   2026       NA   419 Embacher Janis   101060 SC Rappe… SCRJ         Stürmer 
#> 10   2026       NA   418 Ustinkov Daniil  101139 ZSC Lions ZSC          Verteid…
#> # ℹ 417 more rows
#> # ℹ 8 more variables: games_played <int>, goals <int>, assists <int>,
#> #   points <int>, points_per_game <dbl>, pim_total <int>, plus_minus <int>,
#> #   row_type <chr>
# }
```
