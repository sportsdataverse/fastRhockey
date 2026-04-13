# **NHL Standings Season List**

Returns the list of seasons for which standings data is available.

## Usage

``` r
nhl_standings_season()
```

## Value

Returns a data frame with available standings seasons.

## Examples

``` r
# \donttest{
  try(nhl_standings_season())
#> ── NHL Standings Season ─────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:05:57 UTC
#> # A tibble: 108 × 10
#>          id conferences_in_use divisions_in_use point_for_o_tloss_in_use
#>       <int> <lgl>              <lgl>            <lgl>                   
#>  1 19171918 FALSE              FALSE            FALSE                   
#>  2 19181919 FALSE              FALSE            FALSE                   
#>  3 19191920 FALSE              FALSE            FALSE                   
#>  4 19201921 FALSE              FALSE            FALSE                   
#>  5 19211922 FALSE              FALSE            FALSE                   
#>  6 19221923 FALSE              FALSE            FALSE                   
#>  7 19231924 FALSE              FALSE            FALSE                   
#>  8 19241925 FALSE              FALSE            FALSE                   
#>  9 19251926 FALSE              FALSE            FALSE                   
#> 10 19261927 FALSE              TRUE             FALSE                   
#> # ℹ 98 more rows
#> # ℹ 6 more variables: regulation_wins_in_use <lgl>, row_in_use <lgl>,
#> #   standings_end <chr>, standings_start <chr>, ties_in_use <lgl>,
#> #   wildcard_in_use <lgl>
# }
```
