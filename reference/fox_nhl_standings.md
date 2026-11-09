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
#> ℹ Data updated: 2026-11-09 17:19:49 UTC
#> # A tibble: 128 × 24
#>    team_id section    eastern_conference v2      w_l_otl pts   gp    row   sow  
#>    <chr>   <chr>      <chr>              <chr>   <chr>   <chr> <chr> <chr> <chr>
#>  1 1       CONFERENCE 1                  Hurric… 14-3-2  30    19    14    0    
#>  2 1       CONFERENCE 2                  Bruins  13-4-1  27    18    12    1    
#>  3 1       CONFERENCE 3                  Devils  13-5-0  26    18    12    1    
#>  4 1       CONFERENCE 4                  Canadi… 11-3-3  25    17    9     2    
#>  5 1       CONFERENCE 5                  Lightn… 11-4-2  24    17    11    0    
#>  6 1       CONFERENCE 6                  Senato… 11-6-1  23    18    9     2    
#>  7 1       CONFERENCE 7                  Rangers 10-7-1  21    18    10    0    
#>  8 1       CONFERENCE 8                  Panthe… 9-7-2   20    18    9     0    
#>  9 1       CONFERENCE 9                  Sabres  8-6-3   19    17    8     0    
#> 10 1       CONFERENCE 10                 Capita… 9-6-0   18    15    9     0    
#> # ℹ 118 more rows
#> # ℹ 15 more variables: sol <chr>, gf <chr>, ga <chr>, gd <chr>, home <chr>,
#> #   away <chr>, l10 <chr>, strk <chr>, entity_id <chr>,
#> #   western_conference <chr>, east_atlantic <chr>, east_metropolitan <chr>,
#> #   west_central <chr>, west_pacific <chr>, wild_card <chr>
```
