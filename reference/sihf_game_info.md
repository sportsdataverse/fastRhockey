# **SIHF Header of one game**

Header of one game: league, phase, teams, final and per-period score and
shots, venue, attendance, officials.

Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`,
keyless JSON). SIHF keys a season by its END year (2026 = 2025-26). An
unknown `season`, `league_id` or `phase_id` is never an error upstream
(SIHF silently answers another one), so every table answer is checked
against the body's own filter echo and refused when it differs. Every
request is spaced at least one second from the last one to the host.

## Usage

``` r
sihf_game_info(game_id, language = "en")
```

## Arguments

- game_id:

  Required. SIHF game id (game_id in sihf_results), e.g. 20261105000325.

- language:

  Response language. Only part of the body is translated: status,
  position and penalty names stay German.

## Value

A `fastRhockey_data` tibble with 41 columns. A zero-row tibble with
these columns when the payload holds no rows.

|  |  |  |
|----|----|----|
| col_name | type | description |
| game_id | numeric | SIHF game id (numeric join key; the details link of sihf_results, the searchQuery of the game wrappers). |
| season | integer | Season by its END year (2026 = 2025-26). |
| round | integer | Round number within the season. |
| league_id | integer | SIHF league id of the game (1 = National League). |
| league_name | character | League name (e.g., "National League"). |
| region_id | integer | SIHF region id of the game (1 = national). |
| region_name | character | Region name (e.g., "National"). |
| group_id | integer | SIHF group id (0 = no group). |
| group_name | character | Name of the league group the game belongs to ('Keine' = none). |
| tournament_id | integer | SIHF tournament id (1 = championship). |
| tournament_name | character | Tournament name, German (e.g., 'Meisterschaft' = championship). |
| qualification_id | integer | SIHF id of the stage of the season (1 = regular season). |
| qualification_name | character | Stage of the season (e.g., "Regular Season", "Playoff Final"). |
| phase_id | integer | SIHF phase id (regular season, a playoff round, ...). |
| phase_name | character | Phase name (e.g., "NL Playoff Final"). |
| start_date_time | character | Scheduled start, ISO-8601 with UTC offset, as a string. |
| end_date_time | character | End of the game, ISO-8601 with UTC offset, as a string. |
| update_date_time | character | When SIHF last updated the record, ISO-8601 with UTC offset, as a string. |
| status_id | integer | SIHF game-status id (12 = finished). |
| status_name | character | Game-status name; German whatever the language (e.g., "Ende"). |
| status_percent | integer | Game progress, 0-100 (100 once the game is over). |
| status_canceled | logical | Whether the game was cancelled. |
| home_team_id | integer | SIHF team id of the home team. |
| home_team_name | character | Home team display name. |
| home_team_acronym | character | Home team abbreviation. |
| away_team_id | integer | SIHF team id of the away team. |
| away_team_name | character | Away team display name. |
| away_team_acronym | character | Away team abbreviation. |
| home_score | integer | Final home score. |
| away_score | integer | Final away score. |
| home_score_by_period | list | Home goals per period, aligned with period_indicators. |
| away_score_by_period | list | Away goals per period, aligned with period_indicators. |
| home_shots_by_period | list | Home shots on goal per period, aligned with period_indicators. |
| away_shots_by_period | list | Away shots on goal per period, aligned with period_indicators. |
| period_indicators | list | Period labels: "1", "2", "3", then "OT1", ... |
| venue_name | character | Name of the arena the game was played in. |
| venue_city | character | City of the arena with its canton code (e.g., 'Davos-Platz GR'). |
| spectators | integer | Attendance as SIHF reports it. |
| referees | character | Referee names, comma-joined. |
| linesmen | character | Linesman names, comma-joined. |
| broadcasts | character | Broadcasters, comma-joined (e.g., "MYSPORTS,TV24"). |

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
  try(sihf_game_info(game_id = 20261105000325))
#> ── SIHF Game Info Information from data.sihf.ch ─────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 19:41:48 UTC
#> # A tibble: 1 × 41
#>   game_id season round league_id league_name     region_id region_name group_id
#>     <dbl>  <int> <int>     <int> <chr>               <int> <chr>          <int>
#> 1 2.03e13   2026    47         1 National League         1 National           0
#> # ℹ 33 more variables: group_name <chr>, tournament_id <int>,
#> #   tournament_name <chr>, qualification_id <int>, qualification_name <chr>,
#> #   phase_id <int>, phase_name <chr>, start_date_time <chr>,
#> #   end_date_time <chr>, update_date_time <chr>, status_id <int>,
#> #   status_name <chr>, status_percent <int>, status_canceled <lgl>,
#> #   home_team_id <int>, home_team_name <chr>, home_team_acronym <chr>,
#> #   away_team_id <int>, away_team_name <chr>, away_team_acronym <chr>, …
# }
```
