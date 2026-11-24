# **SIHF Games of a league season**

Games of a league season: date, teams, score, per-period score,
decision, status and game_id.

Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`,
keyless JSON). SIHF keys a season by its END year (2026 = 2025-26). An
unknown `season`, `league_id` or `phase_id` is never an error upstream
(SIHF silently answers another one), so every table answer is checked
against the body's own filter echo and refused when it differs. Every
request is spaced at least one second from the last one to the host.

## Usage

``` r
sihf_results(season, league_id = 1L, phase_id = NULL, language = "en")
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

A `fastRhockey_data` tibble with 26 columns. A zero-row tibble with
these columns when the payload holds no rows.

|  |  |  |
|----|----|----|
| col_name | type | description |
| season | integer | Season by its END year (2026 = 2025-26), from the body's echo of the Season filter. |
| phase_id | integer | SIHF phase id (regular season, a playoff round, ...); null unless a phase filter is passed. |
| day | character | Weekday abbreviation, German (e.g., "Di"). |
| date | Date | Game date (local). |
| time | character | Scheduled local start time, "hh:mm". |
| home_team_id | integer | SIHF team id of the home team. |
| home_team_name | character | Home team display name. |
| home_team_acronym | character | Home team abbreviation. |
| away_team_id | integer | SIHF team id of the away team. |
| away_team_name | character | Away team display name. |
| away_team_acronym | character | Away team abbreviation. |
| score_home | integer | Final home score. |
| score_away | integer | Final away score. |
| score_per_period_home | list | Home goals per period (overtime included as its own period). |
| score_per_period_away | list | Away goals per period (overtime included as its own period). |
| decision | character | How the game was decided beyond regulation: "OT" or "SO"; null in regulation. |
| status_id | integer | SIHF game-status id (12 = finished). |
| status_name | character | Game-status name; German whatever the language (e.g., "Ende"). |
| status_percent | integer | Game progress, 0-100 (100 once the game is over). |
| status_canceled | logical | Whether the game was cancelled. |
| start_date_time | character | Scheduled start, ISO-8601 with UTC offset, as a string. |
| end_date_time | character | End of the game, ISO-8601 with UTC offset, as a string. |
| update_date_time | character | When SIHF last updated the record, ISO-8601 with UTC offset, as a string. |
| game_id | numeric | SIHF game id (numeric join key; the details link of sihf_results, the searchQuery of the game wrappers). |
| broadcasts | character | Broadcasters, comma-joined (e.g., "MYSPORTS,TV24"). |
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
  try(sihf_results(season = 2026, league_id = 1))
#> Error in .euro_check_status(resp) : 
#>   data.sihf.ch/statistic/api/cms/table answered HTTP 403:
#> {"Message":"Public access to this API has been discontinued. Write to
#> support@sihf.ch for access."}
# }
```
