# **Czech Extraliga (ELH) per-minute shot totals**

Cumulative shot attempts and shots on goal of one game, per team and
game minute (`visualization/events/{match_id}.json`). `shots` (attempts)
and `shots_on_goal` are running totals up to the end of the minute;
`team_score` is the team's final score. The file's goal list and power
plays are left out (`return_parsed = FALSE` keeps them; goals are in
[`elh_pbp()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp.md)).
The body names no game id: it is the requested `match_id`. The file
starts in 2017-18.

## Usage

``` r
elh_events(match_id, return_parsed = TRUE)
```

## Arguments

- match_id:

  hokej.cz game id (the S3 file name; `hokejcz_game_id` of
  [`elh_game_ids()`](https://fastRhockey.sportsdataverse.org/reference/elh_game_ids.md)
  /
  [`elh_scoreboard()`](https://fastRhockey.sportsdataverse.org/reference/elh_scoreboard.md)),
  e.g. 2928321. Not the onlajny game id.

- return_parsed:

  Parse the payload into a tibble (default `TRUE`). Pass `FALSE` for the
  raw JSON as a list.

## Value

A tibble (`return_parsed = TRUE`; home rows, then away, by minute; zero
rows for a malformed body), else the raw JSON list:

|  |  |  |
|----|----|----|
| col_name | type | description |
| start_date | character | Scheduled start, local YYYY-MM-DD HH:MM. |
| team_side | character | The team's side: home or away. |
| team_name | character | Team full (official) name. |
| team_score | integer | The team's final goals. |
| minute | integer | Game minute (1-60), the end of which the totals run to. |
| shots | integer | Cumulative shot attempts up to the end of the minute. |
| shots_on_goal | integer | Cumulative shots on goal up to the end of the minute. |

## Errors

`fastRhockey_no_data`: a missing file (404) or a file without minutes.
`fastRhockey_asset_fetch`: an error envelope, an empty or non-JSON 200,
or any other non-2xx. `fastRhockey_bad_request`: HTTP 400/422 or a bad
`match_id`.

## See also

Other ELH Functions:
[`elh_game_ids()`](https://fastRhockey.sportsdataverse.org/reference/elh_game_ids.md),
[`elh_list_objects()`](https://fastRhockey.sportsdataverse.org/reference/elh_list_objects.md),
[`elh_pbp()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp.md),
[`elh_pbp_goals()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp_goals.md),
[`elh_pbp_latest()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp_latest.md),
[`elh_pbp_mobile()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp_mobile.md),
[`elh_scoreboard()`](https://fastRhockey.sportsdataverse.org/reference/elh_scoreboard.md),
[`elh_shots()`](https://fastRhockey.sportsdataverse.org/reference/elh_shots.md)

## Examples

``` r
# \donttest{
  try(elh_events(match_id = 2928321))
#> ── ELH Events Information from hokej.cz ────────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-13 01:51:53 UTC
#> # A tibble: 120 × 7
#>    start_date       team_side team_name    team_score minute shots shots_on_goal
#>    <chr>            <chr>     <chr>             <int>  <int> <int>         <int>
#>  1 2026-10-11 15:00 home      HC Sparta P…          3      1     1             0
#>  2 2026-10-11 15:00 home      HC Sparta P…          3      2     4             0
#>  3 2026-10-11 15:00 home      HC Sparta P…          3      3     5             1
#>  4 2026-10-11 15:00 home      HC Sparta P…          3      4     6             2
#>  5 2026-10-11 15:00 home      HC Sparta P…          3      5     6             2
#>  6 2026-10-11 15:00 home      HC Sparta P…          3      6     9             4
#>  7 2026-10-11 15:00 home      HC Sparta P…          3      7    11             5
#>  8 2026-10-11 15:00 home      HC Sparta P…          3      8    13             6
#>  9 2026-10-11 15:00 home      HC Sparta P…          3      9    14             6
#> 10 2026-10-11 15:00 home      HC Sparta P…          3     10    15             7
#> # ℹ 110 more rows
# }
```
