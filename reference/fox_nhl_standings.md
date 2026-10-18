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
#> ℹ Data updated: 2026-10-18 12:05:25 UTC
#> # A tibble: 128 × 24
#>    team_id section    eastern_conference v2      w_l_otl pts   gp    row   sow  
#>    <chr>   <chr>      <chr>              <chr>   <chr>   <chr> <chr> <chr> <chr>
#>  1 1       CONFERENCE 1                  Hurric… 7-1-1   15    9     7     0    
#>  2 1       CONFERENCE 2                  Bruins  7-2-0   14    9     7     0    
#>  3 1       CONFERENCE 3                  Rangers 6-3-1   13    10    6     0    
#>  4 1       CONFERENCE 4                  Lightn… 6-2-0   12    8     6     0    
#>  5 1       CONFERENCE 5                  Senato… 6-3-0   12    9     4     2    
#>  6 1       CONFERENCE 6                  Panthe… 5-2-2   12    9     5     0    
#>  7 1       CONFERENCE 7                  Capita… 5-3-0   10    8     5     0    
#>  8 1       CONFERENCE 8                  Sabres  4-3-2   10    9     4     0    
#>  9 1       CONFERENCE 9                  Island… 4-3-1   9     8     4     0    
#> 10 1       CONFERENCE 10                 Canadi… 4-3-1   9     8     2     2    
#> # ℹ 118 more rows
#> # ℹ 15 more variables: sol <chr>, gf <chr>, ga <chr>, gd <chr>, home <chr>,
#> #   away <chr>, l10 <chr>, strk <chr>, entity_id <chr>,
#> #   western_conference <chr>, east_atlantic <chr>, east_metropolitan <chr>,
#> #   west_central <chr>, west_pacific <chr>, wild_card <chr>
```
