# **SIHF Goaltender shootouts**

Goaltender shootouts: attempts faced, goals against, saves and save
percentage.

Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`,
keyless JSON). SIHF keys a season by its END year (2026 = 2025-26). An
unknown `season`, `league_id` or `phase_id` is never an error upstream
(SIHF silently answers another one), so every table answer is checked
against the body's own filter echo and refused when it differs. Every
request is spaced at least one second from the last one to the host.

## Usage

``` r
sihf_goalie_shootouts(season, league_id = 1L, phase_id = NULL, language = "en")
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
  every phase.

- language:

  Response language. Only part of the body is translated: status,
  position and penalty names stay German.

## Value

A `fastRhockey_data` tibble with 13 columns. A zero-row tibble with
these columns when the payload holds no rows.

|  |  |  |
|----|----|----|
| col_name | type | description |
| season | integer | Season by its END year (2026 = 2025-26), from the body's echo of the Season filter. |
| phase_id | integer | SIHF phase id (regular season, a playoff round, ...); null unless a phase filter is passed. |
| rank | integer | Rank in the table; null on a summary row, and on a goaltender below the qualification line. |
| player | character | Player name as SIHF prints it, last name first (e.g., "Dahlbeck Klas"); season tables carry no player id. |
| team_id | integer | SIHF team id (integer join key); null on a summary row (the pseudo-team 100000). |
| team_name | character | Team display name as SIHF publishes it; "Durchschnitt" on the league-average row, "Total" on the attendance total row. |
| team_acronym | character | Team abbreviation as SIHF publishes it (e.g., "HCD"); null on a summary row. |
| games_played | integer | Games played in the table's scope. |
| pen_shots_against | integer | Shootout/penalty shots faced. |
| pen_shot_goals_against | integer | Shootout/penalty goals allowed. |
| pen_shot_saves | integer | Shootout/penalty shots saved. |
| pen_shot_saves_percent | numeric | Shootout/penalty save percentage. |
| row_type | character | "data" for a regular row; "average" (league average) or "total" (attendance total) for a summary row SIHF mixes into the table. |

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
  try(sihf_goalie_shootouts(season = 2026, league_id = 1))
#> ── SIHF Goalie Shootouts Information from data.sihf.ch ──── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 17:24:09 UTC
#> # A tibble: 35 × 13
#>    season phase_id  rank player      team_id team_name team_acronym games_played
#>     <int>    <int> <int> <chr>         <int> <chr>     <chr>               <int>
#>  1   2026       NA    16 Waeber Lud…  101149 EHC Klot… EHCK                   25
#>  2   2026       NA    15 Pasche Kev…  103141 Lausanne… LHC                    42
#>  3   2026       NA    14 Senn Gilles  101152 HC Ambri… HCAP                   26
#>  4   2026       NA    13 Meyer Robin  102127 SCL Tige… SCL                    22
#>  5   2026       NA    12 Ciaccio Da…  103144 HC Ajoie  HCA                    29
#>  6   2026       NA    11 Charlin St…  103140 Genève-S… GSHC                   66
#>  7   2026       NA    10 Wüthrich P…  101152 HC Ambri… HCAP                   32
#>  8   2026       NA    NA Durchschni…      NA Durchsch… NA                     44
#>  9   2026       NA     9 Boltshause…  102127 SCL Tige… SCL                    29
#> 10   2026       NA     8 Reideborn …  102126 SC Bern   SCB                    39
#> # ℹ 25 more rows
#> # ℹ 5 more variables: pen_shots_against <int>, pen_shot_goals_against <int>,
#> #   pen_shot_saves <int>, pen_shot_saves_percent <dbl>, row_type <chr>
# }
```
