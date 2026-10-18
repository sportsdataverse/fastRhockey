# **KHL Webcaster Players**

Players with stage-scoped stats (one row per player; khl_id is the
khl.ru player id).

## Usage

``` r
khl_webcaster_players(
  player_ids = NULL,
  season = NULL,
  stage = "regular",
  stage_id = NULL,
  locale = "en",
  return_parsed = TRUE
)
```

## Arguments

- player_ids:

  Webcaster player id(s), not khl.ru ids. NULL = every player of the
  stage (a large body).

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
| id | integer | Webcaster id of the `player` object (webcaster key space); join to khl.ru through its khl_id |
| height | integer | Player height in cm |
| weight | integer | Player weight in kg |
| age | integer | Player age in years |
| shirt_number | integer | Jersey number (not an id) |
| khl_id | integer | khl.ru player id of the `player` object (site key space): the join key to khl-rest |
| role | character | Position: goaltender, defensemen or forward |
| role_key | character | Position key: goaltender, defensemen, forward |
| country | character | Country (English with locale=en) |
| image | character | Player photo URL on khl.ru |
| flag_image_url | character | Country flag image URL |
| name | character | Player name (English with locale=en) |
| birthday | integer | Birth date, unix seconds |
| stick | character | Shooting hand: l or r |
| team_id | integer | Webcaster id of the `team` object (webcaster key space); join to khl.ru through its khl_id |
| team_khl_id | integer | khl.ru club id of the `team` object (site key space): the join key to khl-rest |
| team_name | character | Team name as the site prints it |
| team_location | character | Team city / region (English with locale=en) |
| team_image | character | Team logo URL (webcaster thumbnail) |
| team_division | character | Team division name (English with locale=en) |
| team_division_key | character | Team division key (bobrov, tarasov, kharlamov, chernyshev) |
| team_conference | character | Team conference name (West / East) |
| team_conference_key | character | Team conference key (west / east) |
| stats | character | Stage-scoped stat lines |
| teams | character | Teams played for |
| quotes | character | Video clips (webcaster quotes) |
| seasons_count_khl | integer | KHL seasons played |
| seasons_count_team | integer | Seasons played for the current team |
| positions | character | Positions played |

## Details

Endpoint:
`GET https://khl.api.webcaster.pro/api/khl_mobile/players_v2.json`.

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
[`khl_webcaster_players_light()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_players_light.md),
[`khl_webcaster_stages()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_stages.md),
[`khl_webcaster_tables()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_tables.md),
[`khl_webcaster_team()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_team.md),
[`khl_webcaster_teams()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_teams.md)

## Examples

``` r
# \donttest{
  try(khl_webcaster_players(player_ids = "[33405, 1144]", season = 2026))
#> ── KHL Webcaster Players from khl.api.webcaster.pro ── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 05:47:09 UTC
#> # A tibble: 0 × 29
#> # ℹ 29 variables: id <int>, height <int>, weight <int>, age <int>,
#> #   shirt_number <int>, khl_id <int>, role <chr>, role_key <chr>,
#> #   country <chr>, image <chr>, flag_image_url <chr>, name <chr>,
#> #   birthday <int>, stick <chr>, team_id <int>, team_khl_id <int>,
#> #   team_name <chr>, team_location <chr>, team_image <chr>,
#> #   team_division <chr>, team_division_key <chr>, team_conference <chr>,
#> #   team_conference_key <chr>, stats <chr>, teams <chr>, quotes <chr>, …
# }
```
