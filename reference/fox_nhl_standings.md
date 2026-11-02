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
#> ℹ Data updated: 2026-11-02 03:48:13 UTC
#> # A tibble: 128 × 24
#>    team_id section    eastern_conference v2      w_l_otl pts   gp    row   sow  
#>    <chr>   <chr>      <chr>              <chr>   <chr>   <chr> <chr> <chr> <chr>
#>  1 1       CONFERENCE 1                  Hurric… 12-2-2  26    16    12    0    
#>  2 1       CONFERENCE 2                  Bruins  10-4-1  21    15    10    0    
#>  3 1       CONFERENCE 3                  Canadi… 9-3-2   20    14    7     2    
#>  4 1       CONFERENCE 4                  Senato… 9-5-1   19    15    7     2    
#>  5 1       CONFERENCE 5                  Lightn… 8-3-2   18    13    8     0    
#>  6 1       CONFERENCE 6                  Devils  9-5-0   18    14    9     0    
#>  7 1       CONFERENCE 7                  Panthe… 8-4-2   18    14    8     0    
#>  8 1       CONFERENCE 8                  Maple … 8-6-2   18    16    8     0    
#>  9 1       CONFERENCE 9                  Sabres  7-4-3   17    14    7     0    
#> 10 1       CONFERENCE 10                 Capita… 8-5-0   16    13    8     0    
#> # ℹ 118 more rows
#> # ℹ 15 more variables: sol <chr>, gf <chr>, ga <chr>, gd <chr>, home <chr>,
#> #   away <chr>, l10 <chr>, strk <chr>, entity_id <chr>,
#> #   western_conference <chr>, east_atlantic <chr>, east_metropolitan <chr>,
#> #   west_central <chr>, west_pacific <chr>, wild_card <chr>
```
