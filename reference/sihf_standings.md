# **SIHF League table of a season (every game, or home / away games only)**

League table of a season (every game, or home / away games only): rank,
games, points, regulation / overtime / shootout wins and losses, goals,
streak.

Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`,
keyless JSON). SIHF keys a season by its END year (2026 = 2025-26). An
unknown `season`, `league_id` or `phase_id` is never an error upstream
(SIHF silently answers another one), so every table answer is checked
against the body's own filter echo and refused when it differs. Every
request is spaced at least one second from the last one to the host.

## Usage

``` r
sihf_standings(
  season,
  league_id = 1L,
  phase_id = NULL,
  venue = "All",
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
  filter lists them). `NULL` = no phase filter: the regular-season
  table.

- venue:

  Which games the table counts: All (the full table), Home or Away.
  Always sent: without it SIHF answers the away-games table (26 of the
  52 National League games).

- language:

  Response language. Only part of the body is translated: status,
  position and penalty names stay German.

## Value

A `fastRhockey_data` tibble with 20 columns. A zero-row tibble with
these columns when the payload holds no rows.

|  |  |  |
|----|----|----|
| col_name | type | description |
| season | integer | Season by its END year (2026 = 2025-26), from the body's echo of the Season filter. |
| phase_id | integer | SIHF phase id (regular season, a playoff round, ...); null unless a phase filter is passed. |
| rank | integer | Rank in the table; null on a summary row. |
| team_id | integer | SIHF team id (integer join key). |
| team_name | character | Team display name as SIHF publishes it. |
| team_acronym | character | Team abbreviation as SIHF publishes it (e.g., "HCD"). |
| games_played | integer | Games the team played in the selection (every game, or home / away games only, per venue). |
| points | integer | Table points earned in the standings. |
| points_quotient | numeric | Points per game. |
| games_won_in_time | integer | Wins in regulation. |
| games_won_extra_time | integer | Wins in overtime. |
| games_won_penalty | integer | Wins in a shootout. |
| games_lost_penalty | integer | Losses in a shootout. |
| games_lost_extra_time | integer | Losses in overtime. |
| games_lost_in_time | integer | Losses in regulation. |
| goals_for | integer | Goals scored by the team. |
| goals_against | integer | Goals allowed by the team. |
| goals_difference | integer | Goal difference. |
| streak | character | Recent results, newest first, as SIHF prints them (e.g., "W+L-"; "+" regulation, "-" after regulation). |
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
  try(sihf_standings(season = 2026, league_id = 1))
#> Error in .euro_check_status(resp) : 
#>   data.sihf.ch/statistic/api/cms/table answered HTTP 403:
#> {"Message":"Public access to this API has been discontinued. Write to
#> support@sihf.ch for access."}
# }
```
