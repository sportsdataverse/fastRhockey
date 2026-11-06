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
#> ℹ Data updated: 2026-11-06 07:46:43 UTC
#> # A tibble: 128 × 24
#>    team_id section    eastern_conference v2      w_l_otl pts   gp    row   sow  
#>    <chr>   <chr>      <chr>              <chr>   <chr>   <chr> <chr> <chr> <chr>
#>  1 1       CONFERENCE 1                  Hurric… 13-3-2  28    18    13    0    
#>  2 1       CONFERENCE 2                  Bruins  12-4-1  25    17    11    1    
#>  3 1       CONFERENCE 3                  Canadi… 10-3-3  23    16    8     2    
#>  4 1       CONFERENCE 4                  Lightn… 10-3-2  22    15    10    0    
#>  5 1       CONFERENCE 5                  Devils  11-5-0  22    16    10    1    
#>  6 1       CONFERENCE 6                  Senato… 10-6-1  21    17    8     2    
#>  7 1       CONFERENCE 7                  Panthe… 9-5-2   20    16    9     0    
#>  8 1       CONFERENCE 8                  Rangers 9-7-1   19    17    9     0    
#>  9 1       CONFERENCE 9                  Maple … 8-7-2   18    17    8     0    
#> 10 1       CONFERENCE 10                 Sabres  7-5-3   17    15    7     0    
#> # ℹ 118 more rows
#> # ℹ 15 more variables: sol <chr>, gf <chr>, ga <chr>, gd <chr>, home <chr>,
#> #   away <chr>, l10 <chr>, strk <chr>, entity_id <chr>,
#> #   western_conference <chr>, east_atlantic <chr>, east_metropolitan <chr>,
#> #   west_central <chr>, west_pacific <chr>, wild_card <chr>
```
