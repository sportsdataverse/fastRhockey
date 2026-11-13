# **Get Fox Sports NHL standings**

**Get Fox Sports NHL standings**

## Usage

``` r
fox_nhl_standings(team_id)
```

## Arguments

- team_id:

  Fox Bifrost team id (standings of that team's division/conference).

## Value

A `fastRhockey_data` tibble of standings rows (`team_id`, `section`, the
standings columns, `entity_id`).

## Examples

``` r
 try(fox_nhl_standings("1")) 
#> ── Fox Sports NHL standings ─────────────────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 20:17:19 UTC
#> # A tibble: 128 × 24
#>    team_id section    eastern_conference v2      w_l_otl pts   gp    row   sow  
#>    <chr>   <chr>      <chr>              <chr>   <chr>   <chr> <chr> <chr> <chr>
#>  1 1       CONFERENCE 1                  Hurric… 15-3-2  32    20    15    0    
#>  2 1       CONFERENCE 2                  Lightn… 13-4-2  28    19    13    0    
#>  3 1       CONFERENCE 3                  Canadi… 12-3-4  28    19    10    2    
#>  4 1       CONFERENCE 4                  Devils  14-6-0  28    20    13    1    
#>  5 1       CONFERENCE 5                  Bruins  13-5-1  27    19    12    1    
#>  6 1       CONFERENCE 6                  Senato… 12-6-2  26    20    10    2    
#>  7 1       CONFERENCE 7                  Panthe… 11-7-2  24    20    11    0    
#>  8 1       CONFERENCE 8                  Rangers 11-8-1  23    20    11    0    
#>  9 1       CONFERENCE 9                  Capita… 10-7-0  20    17    10    0    
#> 10 1       CONFERENCE 10                 Island… 8-6-4   20    18    7     1    
#> # ℹ 118 more rows
#> # ℹ 15 more variables: sol <chr>, gf <chr>, ga <chr>, gd <chr>, home <chr>,
#> #   away <chr>, l10 <chr>, strk <chr>, entity_id <chr>,
#> #   western_conference <chr>, east_atlantic <chr>, east_metropolitan <chr>,
#> #   west_central <chr>, west_pacific <chr>, wild_card <chr>
```
