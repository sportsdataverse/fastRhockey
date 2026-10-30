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
#> ── Fox Sports NHL standings ────────────────────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-30 00:11:10 UTC
#> # A tibble: 128 × 24
#>    team_id section    eastern_conference v2      w_l_otl pts   gp    row   sow  
#>    <chr>   <chr>      <chr>              <chr>   <chr>   <chr> <chr> <chr> <chr>
#>  1 1       CONFERENCE 1                  Hurric… 10-2-2  22    14    10    0    
#>  2 1       CONFERENCE 2                  Bruins  9-3-1   19    13    9     0    
#>  3 1       CONFERENCE 3                  Canadi… 7-3-2   16    12    5     2    
#>  4 1       CONFERENCE 4                  Devils  8-5-0   16    13    8     0    
#>  5 1       CONFERENCE 5                  Senato… 8-5-0   16    13    6     2    
#>  6 1       CONFERENCE 6                  Panthe… 7-4-2   16    13    7     0    
#>  7 1       CONFERENCE 7                  Maple … 7-6-2   16    15    7     0    
#>  8 1       CONFERENCE 8                  Lightn… 7-3-1   15    11    7     0    
#>  9 1       CONFERENCE 9                  Rangers 7-6-1   15    14    7     0    
#> 10 1       CONFERENCE 10                 Capita… 7-4-0   14    11    7     0    
#> # ℹ 118 more rows
#> # ℹ 15 more variables: sol <chr>, gf <chr>, ga <chr>, gd <chr>, home <chr>,
#> #   away <chr>, l10 <chr>, strk <chr>, entity_id <chr>,
#> #   western_conference <chr>, east_atlantic <chr>, east_metropolitan <chr>,
#> #   west_central <chr>, west_pacific <chr>, wild_card <chr>
```
