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
  try(khl_webcaster_stages())
#> ── KHL Webcaster Stages from khl.api.webcaster.pro ──────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 22:53:52 UTC
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
