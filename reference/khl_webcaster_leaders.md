# **KHL Webcaster Leaders**

Stat leaders of a stage: one row per player per category.

## Usage

``` r
khl_webcaster_leaders(
  season = NULL,
  stage = "regular",
  stage_id = NULL,
  locale = "en",
  return_parsed = TRUE
)
```

## Arguments

- season:

  Season by its END year (2026 = 2025-26), mapped to the KHL webcaster
  stage id through the webcaster season table (khl_webcaster_stages).
  NULL = the current stage.

- stage:

  Which stage of the season: regular or playoff. One of `regular`,
  `playoff`.

- stage_id:

  Webcaster stage id, sent as is instead of season / stage (370 =
  2025-26 regular season; khl_webcaster_stages lists them). Not a khl.ru
  tournament id.

- locale:

  Response language: en or ru. One of `en`, `ru`.

- return_parsed:

  Logical. `TRUE` (default) returns the parsed `fastRhockey_data`
  tibble; `FALSE` returns the raw JSON body as a list (betting fields
  and the caller's IP removed).

## Value

A `fastRhockey_data` tibble (14 columns; zero rows with the same columns
when the payload is empty), or the raw body as a list when
`return_parsed = FALSE`. Raises `fastRhockey_no_data` (nothing there, or
an unknown season / stage), `fastRhockey_asset_fetch` (an error
envelope, an answer for another id than the one asked, or a failed
fetch) and `fastRhockey_bad_request` (an invalid argument or an HTTP 400
/ 422).

|  |  |  |
|----|----|----|
| col_name | types | description |
| category | character | Leader category |
| rank | integer | 1-based position in its table / list |
| id | integer | Webcaster id of the `players` object (webcaster key space); join to khl.ru through its khl_id |
| khl_id | integer | khl.ru player id of the `players` object (site key space): the join key to khl-rest |
| name | character | Name (English with locale=en) |
| image | character | Player photo URL on khl.ru |
| shirt_number | integer | Jersey number (not an id) |
| role_key | character | Position key: goaltender, defensemen, forward |
| team_id | integer | Webcaster id of the `team` object (webcaster key space); join to khl.ru through its khl_id |
| team_khl_id | integer | khl.ru club id of the `team` object (site key space): the join key to khl-rest |
| team_name | character | Team name as the site prints it |
| team_location | character | Team city / region (English with locale=en) |
| pos | character | Leader position in the category, as text ('1' = leader) |
| stats | character | Category value(s) |

## Details

Endpoint:
`GET https://khl.api.webcaster.pro/api/khl_mobile/leaders_v2.json`.

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
[`khl_webcaster_players()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_players.md),
[`khl_webcaster_players_light()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_players_light.md),
[`khl_webcaster_stages()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_stages.md),
[`khl_webcaster_tables()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_tables.md),
[`khl_webcaster_team()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_team.md),
[`khl_webcaster_teams()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_teams.md)

## Examples

``` r
# \donttest{
  try(khl_webcaster_leaders(season = 2026))
#> ── KHL Webcaster Leaders from khl.api.webcaster.pro ── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 06:13:30 UTC
#> # A tibble: 140 × 14
#>    category  rank    id khl_id name          image shirt_number role_key team_id
#>    <chr>    <int> <int>  <int> <chr>         <chr>        <int> <chr>      <int>
#>  1 Points       1 39739  43033 Anas Sam      http…            7 forward       38
#>  2 Points       2  3449  18770 Okulov Konst… http…           17 forward       10
#>  3 Points       3 47255  45401 Limoges Alex  http…           12 forward       38
#>  4 Points       4  3285  19014 Tkachyov Vla… http…           19 forward       30
#>  5 Points       5 18905  19242 Sharipzyanov… http…           44 defense…      10
#>  6 Points       6 31517  34820 Pinchuk Vita… http…           81 forward       38
#>  7 Points       7 46567  45426 Poturalski A… http…           22 forward       10
#>  8 Points       8 41311  43403 Leivo Joshua  http…           90 forward       28
#>  9 Points       9 46995  45421 Sprong Daniel http…           11 forward       56
#> 10 Points      10 38775  39443 Kantserov Ro… http…            8 forward       30
#> # ℹ 130 more rows
#> # ℹ 5 more variables: team_khl_id <int>, team_name <chr>, team_location <chr>,
#> #   pos <chr>, stats <chr>
# }
```
