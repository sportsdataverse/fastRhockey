# **SIHF Team goals by scorer**

Team goals by scorer: forwards, defensemen and import players (totals
and shares), plus shootout-winning goals for and against.

Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`,
keyless JSON). SIHF keys a season by its END year (2026 = 2025-26). An
unknown `season`, `league_id` or `phase_id` is never an error upstream
(SIHF silently answers another one), so every table answer is checked
against the body's own filter echo and refused when it differs. Every
request is spaced at least one second from the last one to the host.

## Usage

``` r
sihf_team_goals_by_position(
  season,
  league_id = 1L,
  phase_id = NULL,
  language = "en"
)
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

A `fastRhockey_data` tibble with 17 columns. A zero-row tibble with
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
| goals_for_fw_df | integer | Goals by forwards and defencemen. |
| goals_for_fw | integer | Goals by forwards. |
| goals_for_fw_percent | numeric | Share of goals by forwards, percent. |
| goals_for_df | integer | Goals by defencemen. |
| goals_for_df_percent | numeric | Share of goals by defencemen, percent. |
| goals_for_im | integer | Goals by import (foreign-licence) players. |
| goals_for_im_percent | numeric | Share of goals by import players, percent. |
| sow_goals_for | integer | Deciding shootout goals scored. |
| sow_goals_against | integer | Deciding shootout goals conceded. |
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
  try(sihf_team_goals_by_position(season = 2026, league_id = 1))
#> ── SIHF Team Goals By Position Information from data.sihf.ch ───────────────────
#> ℹ Data updated: 2026-11-13 21:35:17 UTC
#> # A tibble: 15 × 17
#>    season phase_id  rank team_id team_name             team_acronym games_played
#>     <int>    <int> <int>   <int> <chr>                 <chr>               <int>
#>  1   2026       NA    14  103144 HC Ajoie              HCA                    56
#>  2   2026       NA    13  101149 EHC Kloten            EHCK                   52
#>  3   2026       NA    12  102126 SC Bern               SCB                    60
#>  4   2026       NA    11  102127 SCL Tigers            SCL                    52
#>  5   2026       NA    10  101152 HC Ambri-Piotta       HCAP                   56
#>  6   2026       NA     9  102128 EHC Biel-Bienne       EHCB                   56
#>  7   2026       NA     8  101150 HC Lugano             HCL                    60
#>  8   2026       NA     7  101144 EV Zug                EVZ                    66
#>  9   2026       NA    NA      NA Durchschnitt          NA                     65
#> 10   2026       NA     6  103141 Lausanne HC           LHC                    66
#> 11   2026       NA     5  101060 SC Rapperswil-Jona L… SCRJ                   74
#> 12   2026       NA     4  101139 ZSC Lions             ZSC                    70
#> 13   2026       NA     3  103140 Genève-Servette HC    GSHC                   76
#> 14   2026       NA     2  103138 Fribourg-Gottéron     FRI                    90
#> 15   2026       NA     1  101151 HC Davos              HCD                    86
#> # ℹ 10 more variables: goals_for_fw_df <int>, goals_for_fw <int>,
#> #   goals_for_fw_percent <dbl>, goals_for_df <int>, goals_for_df_percent <dbl>,
#> #   goals_for_im <int>, goals_for_im_percent <dbl>, sow_goals_for <int>,
#> #   sow_goals_against <int>, row_type <chr>
# }
```
