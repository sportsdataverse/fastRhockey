# **KHL Webcaster Team**

One team with roster, stats and news feed (one row; lists as JSON text).

## Usage

``` r
khl_webcaster_team(
  team_id,
  season = NULL,
  stage = "regular",
  stage_id = NULL,
  locale = "en",
  return_parsed = TRUE
)
```

## Arguments

- team_id:

  Required. Webcaster team id (khl_webcaster_teams id), not the khl.ru
  club id.

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

A `fastRhockey_data` tibble (44 columns; zero rows with the same columns
when the payload is empty), or the raw body as a list when
`return_parsed = FALSE`. Raises `fastRhockey_no_data` (nothing there, or
an unknown season / stage), `fastRhockey_asset_fetch` (an error
envelope, an answer for another id than the one asked, or a failed
fetch) and `fastRhockey_bad_request` (an invalid argument or an HTTP 400
/ 422).

|  |  |  |
|----|----|----|
| col_name | types | description |
| id | integer | Webcaster id of the `team` object (webcaster key space); join to khl.ru through its khl_id |
| stage | character | Stage type the team page answers for: regular or playoff |
| khl_id | integer | khl.ru club id of the `team` object (site key space): the join key to khl-rest |
| name | character | Name (English with locale=en) |
| location | character | Team city / region (English with locale=en) |
| feed_items | character | Team news feed, JSON list (type, date, id, title, image) |
| arena_id | integer | Webcaster id of the team's home arena |
| arena_capacity | integer | Arena seating capacity |
| arena_phone | character | Arena phone number as published |
| arena_website | character | Arena website URL |
| arena_name | character | Home arena name (English with locale=en) |
| arena_address | character | Arena street address (English with locale=en) |
| arena_city | character | Arena city (English with locale=en) |
| arena_image | character | Arena photo URL on khl.ru |
| arena_geo_lat | numeric | Arena latitude, decimal degrees |
| arena_geo_long | numeric | Arena longitude, decimal degrees |
| arenas | character | Home arenas, JSON list (id, capacity, phone, website, name, address) |
| calendar_events | character | Team schedule, JSON list of webcaster events |
| recent_events | character | Recent team games, JSON list of webcaster events |
| website | character | Club website URL |
| mail | character | Club e-mail address as published |
| foundation_year | character | Year the club was founded, as text |
| photo | character | Team photo URL on khl.ru |
| phone | character | Club phone number as published |
| head_coach_photo | character | Head coach photo URL on khl.ru |
| head_coach_name | character | Head coach name (English with locale=en) |
| social_networks_tw | character | Club Twitter / X page URL (empty when none) |
| social_networks_vk | character | Club VKontakte page URL |
| social_networks_ok | character | Club Odnoklassniki page URL |
| social_networks_fb | character | Club Facebook page URL (empty when none) |
| social_networks_instagram | character | Club Instagram page URL |
| social_networks_youtube | character | Club YouTube channel URL (empty when none) |
| social_networks_telegram | character | Club Telegram channel URL |
| apps_ios | character | Undocumented provider field `apps_ios`; carried as sent. |
| apps_android | character | Undocumented provider field `apps_android`; carried as sent. |
| apps_windows | character | Undocumented provider field `apps_windows`; carried as sent. |
| address | character | Club office address (English with locale=en) |
| image | character | Team logo URL (webcaster thumbnail) |
| division | character | Division name (English with locale=en) |
| division_key | character | Division key (bobrov, tarasov, kharlamov, chernyshev) |
| conference | character | Conference (west / east) |
| conference_key | character | Conference key (east / west) |
| stats | character | Team stat lines |
| players | character | Roster (webcaster players) |

## Details

Endpoint:
`GET https://khl.api.webcaster.pro/api/khl_mobile/team_v2.json`.

The webcaster API is keyless JSON. Neither backend takes a year:
`season` (the END year, 2026 = 2025/26) and `stage` are mapped to the
webcaster stage id through the season table of
[`khl_webcaster_stages()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_stages.md)
(cached for the R session), or pass `stage_id`. Honours
`getOption("fastRhockey.proxy")`; betting fields and the caller's IP are
removed from every body.

Ids and counts are integers; a column holding values past 2^31 (epoch
milliseconds) is returned as a whole-number double.

At least one second is left between two requests to a host.

Check the provider's terms of use before redistributing this data.

## See also

[family_khl](https://fastRhockey.sportsdataverse.org/reference/family_khl.md)
lists every function in this family.

## Examples

``` r
# \donttest{
  try(khl_webcaster_team(season = 2026, team_id = 18))
#> ── KHL Webcaster Team from khl.api.webcaster.pro ────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 21:29:48 UTC
#> # A tibble: 1 × 44
#>      id stage   khl_id name    location feed_items       arena_id arena_capacity
#>   <int> <chr>    <int> <chr>   <chr>    <chr>               <int>          <int>
#> 1    18 regular      7 Spartak Moscow   "[{\"type\": \"…      469          12396
#> # ℹ 36 more variables: arena_phone <chr>, arena_website <chr>,
#> #   arena_name <chr>, arena_address <chr>, arena_city <chr>, arena_image <chr>,
#> #   arena_geo_lat <dbl>, arena_geo_long <dbl>, arenas <chr>,
#> #   calendar_events <chr>, recent_events <chr>, website <chr>, mail <chr>,
#> #   foundation_year <chr>, photo <chr>, phone <chr>, head_coach_photo <chr>,
#> #   head_coach_name <chr>, social_networks_tw <chr>, social_networks_vk <chr>,
#> #   social_networks_ok <chr>, social_networks_fb <chr>, …
# }
```
