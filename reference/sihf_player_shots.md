# **SIHF Skater shots**

Skater shots: on goal (total, slot, shooting percentage), missed, post
and blocked, with per-game rates.

Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`,
keyless JSON). SIHF keys a season by its END year (2026 = 2025-26). An
unknown `season`, `league_id` or `phase_id` is never an error upstream
(SIHF silently answers another one), so every table answer is checked
against the body's own filter echo and refused when it differs. Every
request is spaced at least one second from the last one to the host.

## Usage

``` r
sihf_player_shots(season, league_id = 1L, phase_id = NULL, language = "en")
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

A `fastRhockey_data` tibble with 21 columns. A zero-row tibble with
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
| sog_sog | integer | Shots on goal (saved or scored). |
| sog_sog_percent | numeric | Shooting percentage (goals / shots on goal). |
| sog_sog_quotient | numeric | Shots on goal per game. |
| sog_sog_slot | integer | Slot shots on goal. |
| sog_sog_slot_per_game | numeric | Slot shots on goal per game. |
| sog_missed | integer | Missed shots, total. |
| sog_missed_per_game | numeric | Missed shots per game. |
| sog_post | integer | Post/crossbar shots, total. |
| sog_post_per_game | numeric | Post/crossbar shots per game. |
| blockings | integer | Number of shots blocked. |
| blockings_per_game | numeric | Blocked shots per game. |
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
  try(sihf_player_shots(season = 2026, league_id = 1))
#> ── SIHF Player Shots Information from data.sihf.ch ──────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 21:35:07 UTC
#> # A tibble: 427 × 21
#>    season phase_id  rank player          team_id team_name team_acronym position
#>     <int>    <int> <int> <chr>             <int> <chr>     <chr>        <chr>   
#>  1   2026       NA   427 Nater Jens       101144 EV Zug    EVZ          Verteid…
#>  2   2026       NA   426 Christen Livio   102128 EHC Biel… EHCB         Stürmer 
#>  3   2026       NA   425 Besanidis Anton  101150 HC Lugano HCL          Stürmer 
#>  4   2026       NA   424 Fuhrer Laurin    101060 SC Rappe… SCRJ         Verteid…
#>  5   2026       NA   423 Baumann Elia     101151 HC Davos  HCD          Stürmer 
#>  6   2026       NA   422 Meier Simon      101149 EHC Klot… EHCK         Stürmer 
#>  7   2026       NA   421 Aeschbach Cédr…  102128 EHC Biel… EHCB         Verteid…
#>  8   2026       NA   420 Ugazzi Nicolò    101060 SC Rappe… SCRJ         Verteid…
#>  9   2026       NA   419 Ryser Vincent    102126 SC Bern   SCB          Stürmer 
#> 10   2026       NA   418 Riatsch Fadri    102126 SC Bern   SCB          Stürmer 
#> # ℹ 417 more rows
#> # ℹ 13 more variables: games_played <int>, sog_sog <int>,
#> #   sog_sog_percent <dbl>, sog_sog_quotient <dbl>, sog_sog_slot <int>,
#> #   sog_sog_slot_per_game <dbl>, sog_missed <int>, sog_missed_per_game <dbl>,
#> #   sog_post <int>, sog_post_per_game <dbl>, blockings <int>,
#> #   blockings_per_game <dbl>, row_type <chr>
# }
```
