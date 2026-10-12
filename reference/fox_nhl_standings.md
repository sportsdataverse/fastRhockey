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
#> ── Fox Sports NHL standings ─────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-10-12 12:06:49 UTC
#> # A tibble: 128 × 24
#>    team_id section    eastern_conference v2      w_l_otl pts   gp    row   sow  
#>    <chr>   <chr>      <chr>              <chr>   <chr>   <chr> <chr> <chr> <chr>
#>  1 1       CONFERENCE 1                  Hurric… 5-1-1   11    7     5     0    
#>  2 1       CONFERENCE 2                  Rangers 5-2-0   10    7     5     0    
#>  3 1       CONFERENCE 3                  Lightn… 4-1-0   8     5     4     0    
#>  4 1       CONFERENCE 4                  Senato… 4-1-0   8     5     3     1    
#>  5 1       CONFERENCE 5                  Bruins  4-2-0   8     6     4     0    
#>  6 1       CONFERENCE 6                  Maple … 3-2-1   7     6     3     0    
#>  7 1       CONFERENCE 7                  Pengui… 3-2-1   7     6     2     1    
#>  8 1       CONFERENCE 8                  Capita… 3-2-0   6     5     3     0    
#>  9 1       CONFERENCE 9                  Panthe… 2-1-2   6     5     2     0    
#> 10 1       CONFERENCE 10                 Canadi… 2-2-1   5     5     1     1    
#> # ℹ 118 more rows
#> # ℹ 15 more variables: sol <chr>, gf <chr>, ga <chr>, gd <chr>, home <chr>,
#> #   away <chr>, l10 <chr>, strk <chr>, entity_id <chr>,
#> #   western_conference <chr>, east_atlantic <chr>, east_metropolitan <chr>,
#> #   west_central <chr>, west_pacific <chr>, wild_card <chr>
```
