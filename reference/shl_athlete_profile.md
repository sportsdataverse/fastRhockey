# Player profile: age, height, weight, position

Wraps `GET https://www.shl.se/api/statistics-v2/athlete/profile-page` of
the SHL site API (www.shl.se). Unofficial use of a public source;
seasons are END years (2026 = 2025/26) and are mapped to the site's ssgt
uuids through `/statistics-v2/layout-info`.

## Usage

``` r
shl_athlete_profile(
  athlete_uuid,
  master_site_instance_id = NULL,
  return_parsed = TRUE
)
```

## Arguments

- athlete_uuid:

  Required. Athlete uuid.

- master_site_instance_id:

  Site instance (shl1_shl). No effect in the drop-one probe.

- return_parsed:

  Parse the payload into a tibble (default `TRUE`); `FALSE` returns the
  raw decoded JSON as a list.

## Value

A `fastRhockey_data` tibble (the raw JSON list when
`return_parsed = FALSE`) with the following columns:

|  |  |  |
|----|----|----|
| col_name | type | description |
| uuid | character | Athlete uuid (platform key space, qQ9-... or 10-char form); joins to the athlete routes and stats-info stats\[\].info.uuid. Not the isa or ramses_legacy id. |
| full_name | character | Given name and surname. |
| first_name | character | Given (first) name of the player. |
| last_name | character | Surname (family name) of the player. |
| birth_date | character | Date of birth (YYYY-MM-DD). |
| nationality | character | Nationality (ISO country code). |
| gender | character | Sex of the athlete or team (male / female). |
| age_value | integer | Age of the player in whole years. |
| age_format | character | Unit of value: years. |
| weight_value | integer | Weight of the player in kilograms. |
| weight_format | character | Unit of value: kg. |
| height_value | integer | Height of the player in centimetres. |
| height_format | character | Unit of value: cm. |
| jersey_number | integer | Jersey (shirt) number of the player. |
| position | character | Position label in Swedish (Målvakt = goalkeeper). |
| position_code | character | Position code (GK, D, F; CE / LW / RW in stats rows); N/A when unknown. |
| shoots | character | Shooting side (L / R; Left / Right). |
| team_uuid | character | Platform team uuid. |
| team_instance_id | character | Club site instance id (e.g. fhc1_fhc). |
| team_name | character | Name of the team. |
| team_code | character | StatNet team code (SAIK, FHC). |
| team_media | character | Media URL or string. |
| season_stats | character | Current-season stat rows of the player (empty in the sample). |
| career_stats | character | Career stat rows of the player (empty in the sample). |
| is_in_squad | logical | Whether the player is in the current squad. |
| media_url | character | Portrait URL on the imgix CDN. |
| media_alt | character | Image alt text. Empty string in every sample (placeholder). |
| media_srcset | character | Responsive image srcset. |
| media_url_img_original_proportion | character | Image URL, original proportion. |
| media_url_placeholder_image | character | Placeholder image URL. Empty string in every sample (placeholder). |
| media_videourl | character | Link to a club video. Empty string in every sample (placeholder). |
| media_photographer | character | Image credit. Empty string in every sample (placeholder). |
| media_focal_point | character | Image crop focal point. |

## Errors

`fastRhockey_no_data`: the host answered 404, an empty body (gameday
routes before 2023/24), a table with no rows, or the season and game
type are not in layout-info. `fastRhockey_asset_fetch`: any other
non-2xx answer or a dead connection after retries, a non-JSON or
error-envelope 200, a stats-info page that reaches `count` or falls
short of its `totalCount`, or a body for another game, team, athlete or
ssgt than the one asked for. `fastRhockey_bad_request`: a 400 / 422, a
season that is not a year, or an argument outside its choices.

## See also

[family_shl](https://fastRhockey.sportsdataverse.org/reference/family_shl.md)
lists every function in this family.

## Examples

``` r
# \donttest{
  try(shl_athlete_profile(athlete_uuid = "qQ9-349c7WDv6", master_site_instance_id = "shl1_shl"))
#> ── SHL Athlete Profile Information from www.shl.se ──────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 21:34:24 UTC
#> # A tibble: 1 × 34
#>   uuid    full_name first_name last_name birth_date nationality gender age_value
#>   <chr>   <chr>     <chr>      <chr>     <chr>      <chr>       <chr>      <int>
#> 1 qQ9-34… Lars Joh… Lars       Johansson 1987-07-11 SE          male          39
#> # ℹ 26 more variables: age_format <chr>, weight_value <int>,
#> #   weight_format <chr>, height_value <int>, height_format <chr>,
#> #   jersey_number <int>, position <chr>, position_code <chr>, shoots <chr>,
#> #   team_uuid <chr>, team_instance_id <chr>, team_name <chr>, team_code <chr>,
#> #   team_media <chr>, season_stats <chr>, career_stats <chr>,
#> #   is_in_squad <lgl>, media_url <chr>, media_alt <chr>, media_srcset <chr>,
#> #   media_url_img_original_proportion <chr>, …
# }
```
