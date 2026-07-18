# **NHL Roster Season**

Returns the list of seasons for which roster data is available for a
given team.

## Usage

``` r
nhl_roster_season(team_abbr)
```

## Arguments

- team_abbr:

  Three-letter team abbreviation (e.g., "TOR", "BOS")

## Value

A data frame (`fastRhockey_data`) with the following columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| season | integer | Season for which roster data is available (8-digit, e.g. 20102011). |
| team_abbr | character | Three-letter team abbreviation. |

## Examples

``` r
# \donttest{
  try(nhl_roster_season(team_abbr = "TOR"))
#> ── NHL Roster Season ────────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-07-18 18:48:21 UTC
#> # A tibble: 98 × 2
#>      season team_abbr
#>       <int> <chr>    
#>  1 19271928 TOR      
#>  2 19281929 TOR      
#>  3 19291930 TOR      
#>  4 19301931 TOR      
#>  5 19311932 TOR      
#>  6 19321933 TOR      
#>  7 19331934 TOR      
#>  8 19341935 TOR      
#>  9 19351936 TOR      
#> 10 19361937 TOR      
#> # ℹ 88 more rows
# }
```
