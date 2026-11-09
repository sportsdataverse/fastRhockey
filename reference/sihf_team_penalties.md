# **SIHF Team penalties per season**

Team penalties per season: 2-, 5- and 10-minute penalties, game
misconducts, match penalties and penalty minutes.

Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`,
keyless JSON). SIHF keys a season by its END year (2026 = 2025-26). An
unknown `season`, `league_id` or `phase_id` is never an error upstream
(SIHF silently answers another one), so every table answer is checked
against the body's own filter echo and refused when it differs. Every
request is spaced at least one second from the last one to the host.

## Usage

``` r
sihf_team_penalties(season, league_id = 1L, phase_id = NULL, language = "en")
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

A `fastRhockey_data` tibble with 14 columns. A zero-row tibble with
these columns when the payload holds no rows.

|  |  |  |
|----|----|----|
| col_name | type | description |
| season | integer | Season by its END year (2026 = 2025-26), from the body's echo of the Season filter. |
| phase_id | integer | SIHF phase id (regular season, a playoff round, ...); null unless a phase filter is passed. |
| rank | integer | Rank in the table; null on a summary row, and on a goaltender below the qualification line. |
| team_id | integer | SIHF team id (integer join key); null on a summary row (the pseudo-team 100000). |
| team_name | character | Team display name as SIHF publishes it; "Durchschnitt" on the league-average row, "Total" on the attendance total row. |
| team_acronym | character | Team abbreviation as SIHF publishes it (e.g., "HCD"); null on a summary row. |
| games_played | integer | Games played in the table's scope. |
| pim2 | integer | Number of 2-minute minor penalties. |
| pim5 | integer | Number of 5-minute major penalties. |
| pim10 | integer | Number of 10-minute misconducts. |
| game_misconducts | integer | Number of game misconducts. |
| match_penalties | integer | Number of match penalties. |
| pim_total | integer | Penalty minutes, total. |
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
  try(sihf_team_penalties(season = 2026, league_id = 1))
#> ── SIHF Team Penalties Information from data.sihf.ch ────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 17:24:32 UTC
#> # A tibble: 15 × 14
#>    season phase_id  rank team_id team_name team_acronym games_played  pim2  pim5
#>     <int>    <int> <int>   <int> <chr>     <chr>               <int> <int> <int>
#>  1   2026       NA    14  101149 EHC Klot… EHCK                   52   148     1
#>  2   2026       NA    13  102127 SCL Tige… SCL                    52   158     2
#>  3   2026       NA    12  102126 SC Bern   SCB                    60   176     3
#>  4   2026       NA    11  102128 EHC Biel… EHCB                   56   205     3
#>  5   2026       NA    10  103144 HC Ajoie  HCA                    56   171     4
#>  6   2026       NA     9  101152 HC Ambri… HCAP                   56   176     4
#>  7   2026       NA     8  101144 EV Zug    EVZ                    66   200     5
#>  8   2026       NA     7  101139 ZSC Lions ZSC                    70   226     3
#>  9   2026       NA     6  101150 HC Lugano HCL                    60   178    10
#> 10   2026       NA    NA      NA Durchsch… NA                     65   224     6
#> 11   2026       NA     5  103141 Lausanne… LHC                    66   251    10
#> 12   2026       NA     4  103140 Genève-S… GSHC                   76   291     8
#> 13   2026       NA     3  101151 HC Davos  HCD                    86   310    11
#> 14   2026       NA     2  103138 Fribourg… FRI                    90   353     9
#> 15   2026       NA     1  101060 SC Rappe… SCRJ                   74   305    12
#> # ℹ 5 more variables: pim10 <int>, game_misconducts <int>,
#> #   match_penalties <int>, pim_total <int>, row_type <chr>
# }
```
