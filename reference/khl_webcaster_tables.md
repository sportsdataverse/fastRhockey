# **KHL Webcaster Tables**

League table of every season (one row per season per team, from the
championship table).

## Usage

``` r
khl_webcaster_tables(locale = "en", return_parsed = TRUE)
```

## Arguments

- locale:

  Response language: en or ru. One of `en`, `ru`.

- return_parsed:

  Logical. `TRUE` (default) returns the parsed `fastRhockey_data`
  tibble; `FALSE` returns the raw JSON body as a list (betting fields
  and the caller's IP removed).

## Value

A `fastRhockey_data` tibble (29 columns; zero rows with the same columns
when the payload is empty), or the raw body as a list when
`return_parsed = FALSE`. Raises `fastRhockey_no_data` (nothing there, or
an unknown season / stage), `fastRhockey_asset_fetch` (an error
envelope, an answer for another id than the one asked, or a failed
fetch) and `fastRhockey_bad_request` (an invalid argument or an HTTP 400
/ 422).

|  |  |  |
|----|----|----|
| col_name | types | description |
| season | integer | Season END year (2026 = 2025-26) |
| season_label | character | Season label as sent (26/27, 2025/2026) |
| rank | integer | 1-based position in its table / list |
| gp | integer | Games played (gp = w + otw + sow + sol + otl + l) |
| w | integer | Regulation wins only: overtime (otw) and shootout (sow) wins are separate columns |
| otw | integer | Overtime wins (not counted in w) |
| sow | integer | Shootout wins (not counted in w) |
| sol | integer | Shootout losses |
| otl | integer | Overtime losses |
| otl_en | integer | Overtime losses with the net empty |
| l | integer | Regulation losses only: overtime (otl) and shootout (sol) losses are separate columns |
| pts | integer | Points: 2 per win of any kind, 1 per overtime or shootout loss (3-2-1 in early seasons such as 2013-14) |
| pts_pct | numeric | Points percentage |
| soa | integer | Shutouts against, 1-7 a season (the refs say shots on goal against; the values do not fit that; unverified) |
| so | integer | Shutouts (refs: khl-returns.md tables_v2) |
| gf | integer | Goals scored by the team |
| ga | integer | Goals allowed by the team |
| pim | integer | Penalty minutes |
| pima | integer | Opponents' penalty minutes |
| id | integer | Webcaster id of the `stages` object (webcaster key space); no khl.ru counterpart in the body |
| khl_id | integer | khl.ru club id of the `championship` object (site key space): the join key to khl-rest |
| image | character | Team logo URL (webcaster thumbnail) |
| tv_image | character | Team logo path on the KHL TV front end |
| name | character | Name (English with locale=en) |
| location | character | Team city / region (English with locale=en) |
| division | character | Division name (English with locale=en) |
| division_key | character | Division key (bobrov, tarasov, kharlamov, chernyshev) |
| conference | character | Conference (west / east) |
| conference_key | character | Conference key (east / west) |

## Details

Endpoint:
`GET https://khl.api.webcaster.pro/api/khl_mobile/tables_v2.json`.

The webcaster API is keyless JSON. Neither backend takes a year:
`season` (the END year, 2026 = 2025/26) and `stage` are mapped to the
webcaster stage id through the season table of
[`khl_webcaster_stages()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_stages.md)
(cached for the R session), or pass `stage_id`. Honours
`getOption("fastRhockey.proxy")`; betting fields and the caller's IP are
removed from every body.

Ids and counts are integers (sdv-py Int64); a column holding values past
2^31 (epoch milliseconds) is returned as a whole-number double.

At least one second is left between two requests to a host.

**Owner flag (sanctions).** The KHL and its webcaster platform are run
by Russian operators, some clubs by sanctioned state companies. Fetching
on your own machine is fine; any published data release, hosted mirror
or redistribution needs owner (sanctions / legal) sign-off first.

## See also

Other KHL Functions:
[`khl_game_header()`](https://fastRhockey.sportsdataverse.org/reference/khl_game_header.md),
[`khl_game_info()`](https://fastRhockey.sportsdataverse.org/reference/khl_game_info.md),
[`khl_game_protocol()`](https://fastRhockey.sportsdataverse.org/reference/khl_game_protocol.md),
[`khl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/khl_pbp.md),
[`khl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/khl_player_box.md),
[`khl_player_career()`](https://fastRhockey.sportsdataverse.org/reference/khl_player_career.md),
[`khl_player_profile()`](https://fastRhockey.sportsdataverse.org/reference/khl_player_profile.md),
[`khl_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/khl_player_stats.md),
[`khl_playoff_bracket()`](https://fastRhockey.sportsdataverse.org/reference/khl_playoff_bracket.md),
[`khl_roster()`](https://fastRhockey.sportsdataverse.org/reference/khl_roster.md),
[`khl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/khl_schedule.md),
[`khl_shots()`](https://fastRhockey.sportsdataverse.org/reference/khl_shots.md),
[`khl_standings()`](https://fastRhockey.sportsdataverse.org/reference/khl_standings.md),
[`khl_stat_filters()`](https://fastRhockey.sportsdataverse.org/reference/khl_stat_filters.md),
[`khl_webcaster_event()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_event.md),
[`khl_webcaster_events()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_events.md),
[`khl_webcaster_leaders()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_leaders.md),
[`khl_webcaster_players()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_players.md),
[`khl_webcaster_players_light()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_players_light.md),
[`khl_webcaster_stages()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_stages.md),
[`khl_webcaster_team()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_team.md),
[`khl_webcaster_teams()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_teams.md)

## Examples

``` r
# \donttest{
  try(khl_webcaster_tables())
#> ── KHL Webcaster Tables from khl.api.webcaster.pro ─── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-30 00:15:15 UTC
#> # A tibble: 468 × 29
#>    season season_label  rank    gp     w   otw   sow   sol   otl otl_en     l
#>     <dbl> <chr>        <int> <int> <int> <int> <int> <int> <int>  <int> <int>
#>  1   2026 2025/2026        1    68    37     8     4     3     4      0    12
#>  2   2026 2025/2026        2    68    36     6     5     1     4      0    16
#>  3   2026 2025/2026        3    68    34     6     6     2     4      0    16
#>  4   2026 2025/2026        4    68    38     2     3     6     2      0    17
#>  5   2026 2025/2026        5    68    33     2     4     5     5      0    19
#>  6   2026 2025/2026        6    68    29     9     2     4     2      0    22
#>  7   2026 2025/2026        7    68    33     3     2     7     1      0    22
#>  8   2026 2025/2026        8    68    32     4     4     0     4      0    24
#>  9   2026 2025/2026        9    68    28     7     2     4     3      0    24
#> 10   2026 2025/2026       10    68    27     6     4     2     5      0    24
#> # ℹ 458 more rows
#> # ℹ 18 more variables: pts <int>, pts_pct <dbl>, soa <int>, so <int>, gf <int>,
#> #   ga <int>, pim <int>, pima <int>, id <int>, khl_id <int>, image <chr>,
#> #   tv_image <chr>, name <chr>, location <chr>, division <chr>,
#> #   division_key <chr>, conference <chr>, conference_key <chr>
# }
```
