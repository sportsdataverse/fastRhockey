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
#> ℹ Data updated: 2026-11-04 04:38:51 UTC
#> # A tibble: 128 × 24
#>    team_id section    eastern_conference v2      w_l_otl pts   gp    row   sow  
#>    <chr>   <chr>      <chr>              <chr>   <chr>   <chr> <chr> <chr> <chr>
#>  1 1       CONFERENCE 1                  Hurric… 12-3-2  26    17    12    0    
#>  2 1       CONFERENCE 2                  Bruins  11-4-1  23    16    10    1    
#>  3 1       CONFERENCE 3                  Canadi… 9-3-3   21    15    7     2    
#>  4 1       CONFERENCE 4                  Senato… 9-6-1   19    16    7     2    
#>  5 1       CONFERENCE 5                  Lightn… 9-3-2   20    14    9     0    
#>  6 1       CONFERENCE 6                  Devils  10-5-0  20    15    9     1    
#>  7 1       CONFERENCE 7                  Panthe… 8-5-2   18    15    8     0    
#>  8 1       CONFERENCE 8                  Maple … 8-7-2   18    17    8     0    
#>  9 1       CONFERENCE 9                  Sabres  7-4-3   17    14    7     0    
#> 10 1       CONFERENCE 10                 Red Wi… 7-6-3   17    16    5     2    
#> # ℹ 118 more rows
#> # ℹ 15 more variables: sol <chr>, gf <chr>, ga <chr>, gd <chr>, home <chr>,
#> #   away <chr>, l10 <chr>, strk <chr>, entity_id <chr>,
#> #   western_conference <chr>, east_atlantic <chr>, east_metropolitan <chr>,
#> #   west_central <chr>, west_pacific <chr>, wild_card <chr>
```
