# **NHL Stats API — Miscellaneous Endpoints**

Queries miscellaneous NHL Stats REST API endpoints such as glossary,
game types, shift charts, and component season.

## Usage

``` r
nhl_stats_misc(endpoint = "glossary", game_id = NULL, lang = "en")
```

## Arguments

- endpoint:

  Character endpoint path, one of: "glossary", "gameType",
  "shiftcharts", "componentSeason"

- game_id:

  Optional game ID (required for "shiftcharts")

- lang:

  Character language code. Default "en".

## Value

Returns a data frame or list with the requested data.

## Examples

``` r
# \donttest{
  try(nhl_stats_misc(endpoint = "glossary"))
#> ── NHL Stats Misc ───────────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-07 06:13:51 UTC
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
