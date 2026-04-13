# **NHL Stats API — Glossary**

Returns the glossary of stat definitions from the NHL Stats REST API
(`https://api.nhle.com/stats/rest/{lang}/glossary`). Useful to look up
what a given abbreviation or statistic (e.g., `PIM`) means.

## Usage

``` r
nhl_stats_glossary(lang = "en")
```

## Arguments

- lang:

  Character language code. Default `"en"`.

## Value

A `fastRhockey_data` tibble of glossary entries, or `NULL` on failure.

## Examples

``` r
# \donttest{
  try(nhl_stats_glossary())
#> ── NHL Stats Glossary ───────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:05:59 UTC
#> # A tibble: 321 × 7
#>       id abbreviation   definition first_season_for_stat full_name language_code
#>    <int> <chr>          <chr>                      <int> <chr>     <chr>        
#>  1  1046 General conce… Teams pla…              20092010 5-on-5 s… en           
#>  2  1047 A              Assists c…              19171918 Assists   en           
#>  3  1048 A (5-on-5)     Player's …              20092010 Assists … en           
#>  4  1049 A/60 (5-on-5)  Player's …              20092010 Assists … en           
#>  5  1050 A/GP           In compar…              19171918 Assists … en           
#>  6  1051 Bench          Bench min…                    NA Bench pe… en           
#>  7  1052 Ctry           A player'…                    NA Birth co… en           
#>  8  1053 BkS            A blocked…              19971998 Blocked … en           
#>  9  1054 BkS/60         Player's …              19971998 Blocked … en           
#> 10  1055 BkS/GP         Blocked s…              19971998 Blocked … en           
#> # ℹ 311 more rows
#> # ℹ 1 more variable: last_updated <chr>
# }
```
