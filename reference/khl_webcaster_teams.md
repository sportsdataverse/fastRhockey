# **KHL Webcaster Teams**

Teams of a stage (one row per team; khl_id is the khl.ru club id).

## Usage

``` r
khl_webcaster_teams(
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

A `fastRhockey_data` tibble (9 columns; zero rows with the same columns
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
| khl_id | integer | khl.ru club id of the `team` object (site key space): the join key to khl-rest |
| name | character | Team name (English with locale=en) |
| location | character | Team city / region (English with locale=en) |
| image | character | Team logo URL (webcaster thumbnail) |
| division | character | Division name (English with locale=en) |
| division_key | character | Division key (bobrov, tarasov, kharlamov, chernyshev) |
| conference | character | Conference (west / east) |
| conference_key | character | Conference key (east/west) |

## Details

Endpoint:
`GET https://khl.api.webcaster.pro/api/khl_mobile/teams_v2.json`.

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
  try(khl_webcaster_teams(season = 2026))
#> ── KHL Webcaster Teams from khl.api.webcaster.pro ───────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 22:53:55 UTC
#> # A tibble: 22 × 9
#>       id khl_id name         location     image division division_key conference
#>    <int>  <int> <chr>        <chr>        <chr> <chr>    <chr>        <chr>     
#>  1    30     37 Metallurg Mg Magnitogorsk http… Kharlam… kharlamov    East      
#>  2    10     34 Avangard     Omsk Region  http… Chernys… chernyshev   East      
#>  3    26      1 Lokomotiv    Yaroslavl    http… Tarasov… tarasov      West      
#>  4    40     53 Ak Bars      Kazan        http… Kharlam… kharlamov    East      
#>  5    38    207 Dinamo Mn    Minsk        http… Tarasov… tarasov      West      
#>  6    42     56 Severstal    Cherepovets  http… Tarasov… tarasov      West      
#>  7    56    190 Avtomobilist Ekaterinburg http… Kharlam… kharlamov    East      
#>  8    16      2 CSKA         Moscow       http… Tarasov… tarasov      West      
#>  9     8    719 Dynamo Msk   Moscow       http… Tarasov… tarasov      West      
#> 10    44     24 SKA          Saint Peter… http… Bobrov … bobrov       West      
#> # ℹ 12 more rows
#> # ℹ 1 more variable: conference_key <chr>
# }
```
