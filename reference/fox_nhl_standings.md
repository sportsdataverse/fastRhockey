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
#> ℹ Data updated: 2026-11-26 22:25:29 UTC
#> # A tibble: 128 × 24
#>    team_id section    eastern_conference v2      w_l_otl pts   gp    row   sow  
#>    <chr>   <chr>      <chr>              <chr>   <chr>   <chr> <chr> <chr> <chr>
#>  1 1       CONFERENCE 1                  Hurric… 19-3-3  41    25    19    0    
#>  2 1       CONFERENCE 2                  Senato… 16-7-3  35    26    14    2    
#>  3 1       CONFERENCE 3                  Devils  17-8-0  34    25    15    2    
#>  4 1       CONFERENCE 4                  Lightn… 15-7-2  32    24    15    0    
#>  5 1       CONFERENCE 5                  Canadi… 14-8-4  32    26    12    2    
#>  6 1       CONFERENCE 6                  Bruins  15-8-1  31    24    14    1    
#>  7 1       CONFERENCE 7                  Panthe… 14-8-3  31    25    14    0    
#>  8 1       CONFERENCE 8                  Island… 12-7-5  29    24    10    2    
#>  9 1       CONFERENCE 9                  Rangers 13-10-1 27    24    13    0    
#> 10 1       CONFERENCE 10                 Sabres  11-10-4 26    25    11    0    
#> # ℹ 118 more rows
#> # ℹ 15 more variables: sol <chr>, gf <chr>, ga <chr>, gd <chr>, home <chr>,
#> #   away <chr>, l10 <chr>, strk <chr>, entity_id <chr>,
#> #   western_conference <chr>, east_atlantic <chr>, east_metropolitan <chr>,
#> #   west_central <chr>, west_pacific <chr>, wild_card <chr>
```
