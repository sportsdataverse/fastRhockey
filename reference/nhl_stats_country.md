# **NHL Stats API — Country Lookup**

Returns the country lookup list from the NHL Stats REST API
(`https://api.nhle.com/stats/rest/{lang}/country`). Used for player
nationality mapping.

## Usage

``` r
nhl_stats_country(lang = "en")
```

## Arguments

- lang:

  Character language code. Default `"en"`.

## Value

A `fastRhockey_data` tibble of countries, or `NULL` on failure.

## Examples

``` r
# \donttest{
  try(nhl_stats_country())
#> ── NHL Stats Country ────────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:05:58 UTC
#> # A tibble: 49 × 11
#>    id    country3code country_code country_name   has_player_stats image_url    
#>    <chr> <chr>        <chr>        <chr>                     <int> <chr>        
#>  1 RUS   RUS          RU           Russia                        1 /images/coun…
#>  2 CAN   CAN          CA           Canada                        1 /images/coun…
#>  3 NGA   NGA          NG           Nigeria                       1 /images/coun…
#>  4 NLD   NLD          NL           Netherlands                   1 /images/coun…
#>  5 NOR   NOR          NO           Norway                        1 /images/coun…
#>  6 KAZ   KAZ          KZ           Kazakhstan                    1 /images/coun…
#>  7 FRA   FRA          FR           France                        1 /images/coun…
#>  8 KOR   KOR          KR           Korea (South)                 1 /images/coun…
#>  9 GBR   GBR          GB           United Kingdom                1 /images/coun…
#> 10 YUG   YUG          YU           Yugoslavia                    0 NA           
#> # ℹ 39 more rows
#> # ℹ 5 more variables: ioc_code <chr>, is_active <int>, nationality_name <chr>,
#> #   olympic_url <chr>, thumbnail_url <chr>
# }
```
