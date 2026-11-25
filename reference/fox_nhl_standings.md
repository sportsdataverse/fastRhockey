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
#> ℹ Data updated: 2026-11-25 06:29:50 UTC
#> # A tibble: 128 × 24
#>    team_id section    eastern_conference v2      w_l_otl pts   gp    row   sow  
#>    <chr>   <chr>      <chr>              <chr>   <chr>   <chr> <chr> <chr> <chr>
#>  1 1       CONFERENCE 1                  Hurric… 18-3-3  39    24    18    0    
#>  2 1       CONFERENCE 2                  Senato… 16-6-3  35    25    14    2    
#>  3 1       CONFERENCE 3                  Lightn… 15-6-2  32    23    15    0    
#>  4 1       CONFERENCE 4                  Devils  16-8-0  32    24    15    1    
#>  5 1       CONFERENCE 5                  Canadi… 14-7-4  32    25    12    2    
#>  6 1       CONFERENCE 6                  Bruins  15-7-1  31    23    14    1    
#>  7 1       CONFERENCE 7                  Panthe… 13-8-3  29    24    13    0    
#>  8 1       CONFERENCE 8                  Rangers 13-9-1  27    23    13    0    
#>  9 1       CONFERENCE 9                  Island… 11-7-5  27    23    10    1    
#> 10 1       CONFERENCE 10                 Sabres  10-10-4 24    24    10    0    
#> # ℹ 118 more rows
#> # ℹ 15 more variables: sol <chr>, gf <chr>, ga <chr>, gd <chr>, home <chr>,
#> #   away <chr>, l10 <chr>, strk <chr>, entity_id <chr>,
#> #   western_conference <chr>, east_atlantic <chr>, east_metropolitan <chr>,
#> #   west_central <chr>, west_pacific <chr>, wild_card <chr>
```
