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
#> ℹ Data updated: 2026-09-26 19:58:31 UTC
#> # A tibble: 32 × 19
#>    team_id section   eastern_conference v2       w_l_otl pts   gp    row   sow  
#>    <chr>   <chr>     <chr>              <chr>    <chr>   <chr> <chr> <chr> <chr>
#>  1 1       PRESEASON 1                  Canadie… 3-0-0   6     3     3     0    
#>  2 1       PRESEASON 2                  Red Win… 2-0-1   5     3     2     0    
#>  3 1       PRESEASON 3                  Panthers 2-0-1   5     3     2     0    
#>  4 1       PRESEASON 4                  Devils   2-0-1   5     3     2     0    
#>  5 1       PRESEASON 5                  Capitals 2-0-1   5     3     2     0    
#>  6 1       PRESEASON 6                  Maple L… 2-1-1   5     4     2     0    
#>  7 1       PRESEASON 7                  Bruins   2-1-1   5     4     1     1    
#>  8 1       PRESEASON 8                  Blue Ja… 2-1-0   4     3     1     1    
#>  9 1       PRESEASON 9                  Hurrica… 2-1-0   4     3     1     1    
#> 10 1       PRESEASON 10                 Rangers  2-2-0   4     4     1     1    
#> # ℹ 22 more rows
#> # ℹ 10 more variables: sol <chr>, gf <chr>, ga <chr>, gd <chr>, home <chr>,
#> #   away <chr>, l10 <chr>, strk <chr>, entity_id <chr>,
#> #   western_conference <chr>
```
