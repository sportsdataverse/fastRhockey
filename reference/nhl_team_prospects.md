# **NHL Team Prospects**

Returns prospect information for a given team.

## Usage

``` r
nhl_team_prospects(team_abbr)
```

## Arguments

- team_abbr:

  Three-letter team abbreviation (e.g., "TOR", "BOS")

## Value

A data frame (`fastRhockey_data`) with the following columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| id | integer | Unique player identifier. |
| headshot | character | URL to the prospect's headshot image. |
| position_code | character | Player position code. |
| shoots_catches | character | Handedness (shoots/catches). |
| height_in_inches | integer | Height in inches. |
| weight_in_pounds | integer | Weight in pounds. |
| height_in_centimeters | integer | Height in centimeters. |
| weight_in_kilograms | integer | Weight in kilograms. |
| birth_date | character | Player birth date. |
| birth_country | character | Player birth country. |
| sweater_number | integer | Jersey number. |
| first_name_default | character | First name (default localization). |
| first_name_cs | character | First name (Czech localization). |
| first_name_sk | character | First name (Slovak localization). |
| last_name_default | character | Last name (default localization). |
| last_name_cs | character | Last name (Czech localization). |
| last_name_sk | character | Last name (Slovak localization). |
| birth_city_default | character | Birth city (default localization). |
| birth_city_cs | character | Birth city (Czech localization). |
| birth_city_de | character | Birth city (German localization). |
| birth_city_fi | character | Birth city (Finnish localization). |
| birth_city_fr | character | Birth city (French localization). |
| birth_city_sk | character | Birth city (Slovak localization). |
| birth_city_sv | character | Birth city (Swedish localization). |
| birth_state_province_default | character | Birth state/province (default localization). |
| birth_state_province_fr | character | Birth state/province (French localization). |
| birth_state_province_sk | character | Birth state/province (Slovak localization). |
| birth_state_province_sv | character | Birth state/province (Swedish localization). |
| prospect_group | character | Prospect position group the player belongs to. |
| team_abbr | character | Team abbreviation. |

## Examples

``` r
# \donttest{
  try(nhl_team_prospects(team_abbr = "TOR"))
#> ── NHL Team Prospects ───────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-06-13 07:21:04 UTC
#> # A tibble: 6 × 30
#>       id headshot position_code shoots_catches height_in_inches weight_in_pounds
#>    <int> <chr>    <chr>         <chr>                     <int>            <int>
#> 1 8.48e6 https:/… L             L                            76              223
#> 2 8.48e6 https:/… C             R                            70              173
#> 3 8.48e6 https:/… L             L                            73              187
#> 4 8.48e6 https:/… R             L                            72              176
#> 5 8.48e6 https:/… L             L                            69              180
#> 6 8.48e6 https:/… G             L                            75              212
#> # ℹ 24 more variables: height_in_centimeters <int>, weight_in_kilograms <int>,
#> #   birth_date <chr>, birth_country <chr>, sweater_number <int>,
#> #   first_name_default <chr>, first_name_cs <chr>, first_name_sk <chr>,
#> #   last_name_default <chr>, last_name_cs <chr>, last_name_sk <chr>,
#> #   birth_city_default <chr>, birth_city_cs <chr>, birth_city_de <chr>,
#> #   birth_city_fi <chr>, birth_city_fr <chr>, birth_city_sk <chr>,
#> #   birth_city_sv <chr>, birth_state_province_default <chr>, …
# }
```
