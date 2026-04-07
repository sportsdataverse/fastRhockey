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

Returns a data frame with prospect player data.

## Examples

``` r
# \donttest{
  try(nhl_team_prospects(team_abbr = "TOR"))
#> ── NHL Team Prospects ───────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-07 07:15:54 UTC
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
