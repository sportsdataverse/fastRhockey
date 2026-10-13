# **Czech Extraliga (ELH) scoreboard**

Extraliga games of one recent day with both id spaces (hokej.cz and
onlajny) for games and teams
(`json.esports.cz/hokejcz/scoreboard/{date}.json`). The scoreboard is
the only place hokej.cz ids meet onlajny ids, which is how
[`elh_pbp()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp.md)
joins
[`elh_shots()`](https://fastRhockey.sportsdataverse.org/reference/elh_shots.md).
The window is ROLLING (recent days only), so it cannot map past seasons:
an older day (and today) answers 404. Only the Tipsport extraliga
(league 101) is kept; betting odds are left out (`return_parsed = FALSE`
keeps every league and the odds).

## Usage

``` r
elh_scoreboard(date, return_parsed = TRUE)
```

## Arguments

- date:

  Day, `YYYY-MM-DD` (a character string or a `Date`).

- return_parsed:

  Parse the payload into a tibble (default `TRUE`). Pass `FALSE` for the
  raw JSON as a list.

## Value

A tibble (`return_parsed = TRUE`), else the raw JSON list:

|  |  |  |
|----|----|----|
| col_name | type | description |
| league_id | integer | Scoreboard league key: 101 Tipsport extraliga (ELH), 82 Maxa liga, 130 para hockey. |
| league_name | character | League name, e.g. Tipsport extraliga. |
| hokejcz_game_id | integer | hokej.cz game id (the S3 file name; shots match.id, scoreboard hokejcz_id). integer. |
| onlajny_game_id | integer | Onlajny game id; the scoreboard is the only place it meets the hokej.cz game id. |
| game_date | Date | Game date (shots: YYYY-MM-DD string; scoreboard: Date). |
| start_time | character | Start time, local HH:MM. |
| status_code | character | Game state code: K = finished (others unobserved). |
| status_name | character | Game state in Czech, e.g. konec zapasu (end of game). |
| match_status | character | Status in Czech, e.g. po zapase (after the game). |
| home_score | integer | Home goals (after the entry on play-by-play rows; final on the scoreboard). |
| away_score | integer | Away goals (after the entry on play-by-play rows; final on the scoreboard). |
| score_periods | character | Per-period scores home:away, comma-joined, e.g. 2:0,0:0,1:0. |
| hokejcz_home_team_id | integer | hokej.cz team id of the home team (Sparta = 8); NOT the onlajny id of the play-by-play. |
| onlajny_home_team_id | integer | Onlajny team id of the home team (Sparta = 156); joins the play-by-play onlajny_team_id. |
| home_team_name | character | Home team full name. |
| home_team_short_name | character | Home team short name, e.g. Sparta. |
| home_team_code | character | Home team code, e.g. SPA. |
| hokejcz_away_team_id | integer | hokej.cz team id of the away team (Vitkovice = 7); NOT the onlajny id of the play-by-play. |
| onlajny_away_team_id | integer | Onlajny team id of the away team (Vitkovice = 168); joins the play-by-play onlajny_team_id. |
| away_team_name | character | Away team full name. |
| away_team_short_name | character | Away team short name. |
| away_team_code | character | Away team code, e.g. VIT. |
| stream_url | character | Stream code; md in every sample. |

## Errors

`fastRhockey_no_data`: a day outside the rolling window (404) or a day
without an Extraliga game. `fastRhockey_asset_fetch`: an error envelope,
a body dated another day, an empty or non-JSON 200, or any other
non-2xx. `fastRhockey_bad_request`: HTTP 400/422 or a malformed `date`.

## See also

Other ELH Functions:
[`elh_events()`](https://fastRhockey.sportsdataverse.org/reference/elh_events.md),
[`elh_game_ids()`](https://fastRhockey.sportsdataverse.org/reference/elh_game_ids.md),
[`elh_list_objects()`](https://fastRhockey.sportsdataverse.org/reference/elh_list_objects.md),
[`elh_pbp()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp.md),
[`elh_pbp_goals()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp_goals.md),
[`elh_pbp_latest()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp_latest.md),
[`elh_pbp_mobile()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp_mobile.md),
[`elh_shots()`](https://fastRhockey.sportsdataverse.org/reference/elh_shots.md)

## Examples

``` r
# \donttest{
  try(elh_scoreboard(date = format(Sys.Date() - 1)))
#> Error in .euro_check_status(resp) : 
#>   json.esports.cz/hokejcz/scoreboard/2026-10-12.json answered HTTP 404.
# }
```
