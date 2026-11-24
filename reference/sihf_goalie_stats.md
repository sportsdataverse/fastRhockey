# **SIHF Goaltender season totals**

Goaltender season totals: games, starts, goals against, GAA, shots,
saves, save percentage, ice time.

Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`,
keyless JSON). SIHF keys a season by its END year (2026 = 2025-26). An
unknown `season`, `league_id` or `phase_id` is never an error upstream
(SIHF silently answers another one), so every table answer is checked
against the body's own filter echo and refused when it differs. Every
request is spaced at least one second from the last one to the host.

## Usage

``` r
sihf_goalie_stats(season, league_id = 1L, phase_id = NULL, language = "en")
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

A `fastRhockey_data` tibble with 19 columns. A zero-row tibble with
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
| games_played | integer | Games the goaltender dressed for. |
| first_keeper | integer | Games listed as the starting goaltender. |
| goals_against | integer | Goals allowed by the goaltender. |
| goals_against_average | numeric | Goals against per 60 minutes. |
| shots_against | integer | Shots on goal faced. |
| saves | integer | Shots on goal saved by the goaltender. |
| saves_percentage | numeric | Save percentage on a 0-100 scale (91.25 = .9125). |
| seconds_played | character | Ice time as "mm:ss" (a string, despite the upstream name). Minutes are not capped at 59: an overtime game reads "65:00", a season total "3615:02". |
| penalty_in_minutes | integer | Penalty minutes. |
| goals | integer | Goals scored by the goaltender (almost always 0). |
| assists | integer | Assists by the goaltender. |
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
  try(sihf_goalie_stats(season = 2026, league_id = 1))
#> Error in .euro_check_status(resp) : 
#>   data.sihf.ch/statistic/api/cms/table answered HTTP 403:
#> {"Message":"Public access to this API has been discontinued. Write to
#> support@sihf.ch for access."}
# }
```
