# **SIHF Team goals for and against per season**

Team goals for and against per season: totals and per game, even
strength, power play and short-handed.

Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`,
keyless JSON). SIHF keys a season by its END year (2026 = 2025-26). An
unknown `season`, `league_id` or `phase_id` is never an error upstream
(SIHF silently answers another one), so every table answer is checked
against the body's own filter echo and refused when it differs. Every
request is spaced at least one second from the last one to the host.

## Usage

``` r
sihf_team_goals(season, league_id = 1L, phase_id = NULL, language = "en")
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

A `fastRhockey_data` tibble with 24 columns. A zero-row tibble with
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
| goals_for | integer | Goals scored (goals for). |
| goals_for_quotient | numeric | Goals for per game. |
| goals_against | integer | Goals conceded (goals against). |
| goals_against_quotient | numeric | Goals against per game. |
| even_goals_for | integer | Even-strength goals for. |
| even_goals_for_quotient | numeric | Even-strength goals for per game. |
| even_goals_against | integer | Even-strength goals against. |
| even_goals_against_quotient | numeric | Even-strength goals against per game. |
| pp_goals_for | integer | Power-play goals for. |
| pp_goals_for_quotient | numeric | Power-play goals for per game. |
| pp_goals_against | integer | Goals against while on the power play. |
| pp_goals_against_quotient | numeric | Goals against while on the power play, per game. |
| sh_goals | integer | Short-handed goals for. |
| sh_goals_quotient | numeric | Short-handed goals for per game. |
| pk_goals_against | integer | Goals against while short-handed. |
| pk_goals_against_quotient | numeric | Goals against while short-handed, per game. |
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
  try(sihf_team_goals(season = 2026, league_id = 1))
#> Error in .euro_check_status(resp) : 
#>   data.sihf.ch/statistic/api/cms/table answered HTTP 403:
#> {"Message":"Public access to this API has been discontinued. Write to
#> support@sihf.ch for access."}
# }
```
