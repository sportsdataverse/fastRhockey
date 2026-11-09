# **SIHF Skater faceoffs by zone (offensive, neutral, defensive)**

Skater faceoffs by zone (offensive, neutral, defensive): taken, won,
lost and win percentage.

Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`,
keyless JSON). SIHF keys a season by its END year (2026 = 2025-26). An
unknown `season`, `league_id` or `phase_id` is never an error upstream
(SIHF silently answers another one), so every table answer is checked
against the body's own filter echo and refused when it differs. Every
request is spaced at least one second from the last one to the host.

## Usage

``` r
sihf_player_faceoffs_by_zone(
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

A `fastRhockey_data` tibble with 22 columns. A zero-row tibble with
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
| position | character | Position, German whatever the language (e.g., "Stürmer"); null when SIHF prints "-". |
| games_played | integer | Games played in the table's scope. |
| faceoffs_offensive | integer | Faceoffs taken, offensive zone. |
| faceoffs_offensive_percent | numeric | Faceoff win percentage, offensive zone. |
| faceoffs_won_offensive | integer | Faceoffs won, offensive zone. |
| faceoffs_lost_offensive | integer | Faceoffs lost, offensive zone. |
| faceoffs_neutral | integer | Faceoffs taken, neutral zone. |
| faceoffs_neutral_percent | numeric | Faceoff win percentage, neutral zone. |
| faceoffs_won_neutral | integer | Faceoffs won, neutral zone. |
| faceoffs_lost_neutral | integer | Faceoffs lost, neutral zone. |
| faceoffs_defensive | integer | Faceoffs taken, defensive zone. |
| faceoffs_defensive_percent | numeric | Faceoff win percentage, defensive zone. |
| faceoffs_won_defensive | integer | Faceoffs won, defensive zone. |
| faceoffs_lost_defensive | integer | Faceoffs lost, defensive zone. |
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
  try(sihf_player_faceoffs_by_zone(season = 2026, league_id = 1))
#> ── SIHF Player Faceoffs By Zone Information from data.sihf.ch ──────────────────
#> ℹ Data updated: 2026-11-09 17:24:16 UTC
#> # A tibble: 427 × 22
#>    season phase_id  rank player          team_id team_name team_acronym position
#>     <int>    <int> <int> <chr>             <int> <chr>     <chr>        <chr>   
#>  1   2026       NA   427 Zadina Filip     101151 HC Davos  HCD          Stürmer 
#>  2   2026       NA   426 Rathgeb Yannick  103138 Fribourg… FRI          Verteid…
#>  3   2026       NA   425 Puljujärvi Jes…  103140 Genève-S… GSHC         Stürmer 
#>  4   2026       NA   424 Dahlbeck Klas    101151 HC Davos  HCD          Verteid…
#>  5   2026       NA   423 Rutta Jan        103140 Genève-S… GSHC         Verteid…
#>  6   2026       NA   422 Andersson Calle  101151 HC Davos  HCD          Verteid…
#>  7   2026       NA   421 Kapla Michael    103138 Fribourg… FRI          Verteid…
#>  8   2026       NA   420 Knak Simon       101151 HC Davos  HCD          Stürmer 
#>  9   2026       NA   419 Balcers Rudolfs  101139 ZSC Lions ZSC          Stürmer 
#> 10   2026       NA   418 Sörensen Marcus  103138 Fribourg… FRI          Stürmer 
#> # ℹ 417 more rows
#> # ℹ 14 more variables: games_played <int>, faceoffs_offensive <int>,
#> #   faceoffs_offensive_percent <dbl>, faceoffs_won_offensive <int>,
#> #   faceoffs_lost_offensive <int>, faceoffs_neutral <int>,
#> #   faceoffs_neutral_percent <dbl>, faceoffs_won_neutral <int>,
#> #   faceoffs_lost_neutral <int>, faceoffs_defensive <int>,
#> #   faceoffs_defensive_percent <dbl>, faceoffs_won_defensive <int>, …
# }
```
