# **SIHF Goaltender shots against**

Goaltender shots against: on goal (total, slot), missed, post, with
per-game rates and the slot save percentage.

Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`,
keyless JSON). SIHF keys a season by its END year (2026 = 2025-26). An
unknown `season`, `league_id` or `phase_id` is never an error upstream
(SIHF silently answers another one), so every table answer is checked
against the body's own filter echo and refused when it differs. Every
request is spaced at least one second from the last one to the host.

## Usage

``` r
sihf_goalie_shots(season, league_id = 1L, phase_id = NULL, language = "en")
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

A `fastRhockey_data` tibble with 18 columns. A zero-row tibble with
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
| soga_sog | integer | Shots on goal against, total. |
| soga_sog_per_game | numeric | Shots on goal against per game. |
| soga_sog_slot | integer | Slot shots on goal against. |
| soga_sog_slot_per_game | numeric | Slot shots on goal against per game. |
| soga_missed | integer | Missed shots against. |
| soga_missed_per_game | numeric | Missed shots against per game. |
| soga_post | integer | Post/crossbar shots against. |
| soga_post_per_game | numeric | Post/crossbar shots against per game. |
| saves_slot_percentage | numeric | Save percentage on slot shots. |
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
  try(sihf_goalie_shots(season = 2026, league_id = 1))
#> ── SIHF Goalie Shots Information from data.sihf.ch ──────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 22:59:02 UTC
#> # A tibble: 35 × 18
#>    season phase_id  rank player      team_id team_name team_acronym games_played
#>     <int>    <int> <int> <chr>         <int> <chr>     <chr>               <int>
#>  1   2026       NA    16 Meyer Robin  102127 SCL Tige… SCL                    22
#>  2   2026       NA    15 Waeber Lud…  101149 EHC Klot… EHCK                   25
#>  3   2026       NA    14 Senn Gilles  101152 HC Ambri… HCAP                   26
#>  4   2026       NA    13 Boltshause…  102127 SCL Tige… SCL                    29
#>  5   2026       NA    12 Wüthrich P…  101152 HC Ambri… HCAP                   32
#>  6   2026       NA    11 Ciaccio Da…  103144 HC Ajoie  HCA                    29
#>  7   2026       NA    10 Reideborn …  102126 SC Bern   SCB                    39
#>  8   2026       NA     9 Schlegel N…  101150 HC Lugano HCL                    42
#>  9   2026       NA     8 Pasche Kev…  103141 Lausanne… LHC                    42
#> 10   2026       NA    NA Durchschni…      NA Durchsch… NA                     44
#> # ℹ 25 more rows
#> # ℹ 10 more variables: soga_sog <int>, soga_sog_per_game <dbl>,
#> #   soga_sog_slot <int>, soga_sog_slot_per_game <dbl>, soga_missed <int>,
#> #   soga_missed_per_game <dbl>, soga_post <int>, soga_post_per_game <dbl>,
#> #   saves_slot_percentage <dbl>, row_type <chr>
# }
```
