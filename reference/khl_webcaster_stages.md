# **KHL Webcaster Stages**

Season table: every stage (season END year, regular / playoff) with its
khl.ru tournament id and webcaster stage id.

## Usage

``` r
khl_webcaster_stages(locale = "en", return_parsed = TRUE)
```

## Arguments

- locale:

  Response language: en or ru. One of `en`, `ru`.

- return_parsed:

  Logical. `TRUE` (default) returns the parsed `fastRhockey_data`
  tibble; `FALSE` returns the raw JSON body as a list (betting fields
  and the caller's IP removed).

## Value

A `fastRhockey_data` tibble (8 columns; zero rows with the same columns
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
| type | character | regular or playoff |
| tournament_id | integer | khl.ru tournament (stage) id, tnId: 1369 = 2025-26 regular season, 1370 its playoff; equals webcaster stage khl_id |
| stage_id | integer | Webcaster stage id (its own id space; not a khl.ru tournament id) |
| season_label | character | Season label as sent (26/27, 2025/2026) |
| title | character | Stage title (Regular / Playoff) |
| display_rule | character | Front-end display rule key |
| percent_points_scored | logical | Front-end flag: rank the stage by points percentage (False in every stage captured; unverified) |

## Details

Endpoint: `GET https://khl.api.webcaster.pro/api/khl_mobile/data.json`.

The webcaster API is keyless JSON. Neither backend takes a year:
`season` (the END year, 2026 = 2025/26) and `stage` are mapped to the
webcaster stage id through the season table of `khl_webcaster_stages()`
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
[`khl_webcaster_tables()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_tables.md),
[`khl_webcaster_team()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_team.md),
[`khl_webcaster_teams()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_teams.md)

## Examples

``` r
# \donttest{
  try(khl_webcaster_stages())
#> ── KHL Webcaster Stages from khl.api.webcaster.pro ─── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 05:47:11 UTC
#> # A tibble: 39 × 8
#>    season type    tournament_id stage_id season_label title   display_rule
#>     <int> <chr>           <int>    <int> <chr>        <chr>   <chr>       
#>  1   2027 regular          1436      407 2026/2027    Regular default     
#>  2   2026 playoff          1370      395 2025/2026    Playoff playoff_2024
#>  3   2026 regular          1369      370 2025/2026    Regular default     
#>  4   2025 playoff          1289      359 2024/2025    Playoff playoff_2024
#>  5   2025 regular          1288      323 2024/2025    Regular default     
#>  6   2024 playoff          1218      299 2023/2024    Playoff playoff_2024
#>  7   2024 regular          1217      275 2023/2024    Regular default     
#>  8   2023 playoff          1155      259 2022/2023    Playoff default     
#>  9   2023 regular          1154      235 2022/2023    Regular default     
#> 10   2022 playoff          1098      225 2021/2022    Playoff default     
#> # ℹ 29 more rows
#> # ℹ 1 more variable: percent_points_scored <lgl>
# }
```
