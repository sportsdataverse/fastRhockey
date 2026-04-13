# **NHL Stats API — Skater Milestones**

Returns skater milestone achievements from the NHL Stats REST API
(`https://api.nhle.com/stats/rest/{lang}/milestones/skaters`).

## Usage

``` r
nhl_stats_skater_milestones(
  lang = "en",
  cayenne_exp = NULL,
  limit = 100,
  start = 0
)
```

## Arguments

- lang:

  Character language code. Default `"en"`.

- cayenne_exp:

  Optional Cayenne filter expression string passed via the `cayenneExp`
  query parameter.

- limit:

  Integer maximum number of results. Default 100.

- start:

  Integer pagination start index. Default 0.

## Value

A `fastRhockey_data` tibble of skater milestones, or `NULL` on failure.

## Examples

``` r
# \donttest{
  try(nhl_stats_skater_milestones())
#> ── NHL Stats Skater Milestones ──────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:06:03 UTC
#> # A tibble: 100 × 17
#>       id assists current_team_id first_name   game_type_id games_played goals
#>    <int>   <int>           <int> <chr>               <int>        <int> <int>
#>  1  1227     199              16 Ryan                    2          566    76
#>  2  1358      48               6 David                   3           90    39
#>  3  1360      19               2 Jean-Gabriel            3           86    24
#>  4  1576      45               5 Erik                    3           67     8
#>  5  1756      20              54 Tomas                   3           80    28
#>  6  1776      33              18 Roman                   3           91    12
#>  7  2393      30               2 Brayden                 3           82    13
#>  8  2402      45              14 Brayden                 3           92    44
#>  9  2418      31              30 Marcus                  3          114    16
#> 10  2423      27              16 Andre                   3           93    20
#> # ℹ 90 more rows
#> # ℹ 10 more variables: last_name <chr>, milestone <chr>,
#> #   milestone_amount <int>, player_full_name <chr>, player_id <int>,
#> #   points <int>, team_abbrev <chr>, team_common_name <chr>,
#> #   team_full_name <chr>, team_place_name <chr>
# }
```
