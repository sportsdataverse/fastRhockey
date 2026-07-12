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
#> ℹ Data updated: 2026-07-12 18:40:23 UTC
#> # A tibble: 128 × 24
#>    team_id section    eastern_conference v2      w_l_otl pts   gp    row   sow  
#>    <chr>   <chr>      <chr>              <chr>   <chr>   <chr> <chr> <chr> <chr>
#>  1 1       CONFERENCE 1                  Hurric… 53-22-7 113   82    48    5    
#>  2 1       CONFERENCE 2                  Sabres  50-23-9 109   82    45    5    
#>  3 1       CONFERENCE 3                  Lightn… 50-26-6 106   82    46    4    
#>  4 1       CONFERENCE 4                  Canadi… 48-24-… 106   82    44    4    
#>  5 1       CONFERENCE 5                  Bruins  45-27-… 100   82    41    4    
#>  6 1       CONFERENCE 6                  Senato… 44-27-… 99    82    41    3    
#>  7 1       CONFERENCE 7                  Pengui… 41-25-… 98    82    38    3    
#>  8 1       CONFERENCE 8                  Flyers  43-27-… 98    82    33    10   
#>  9 1       CONFERENCE 9                  Capita… 43-30-9 95    82    41    2    
#> 10 1       CONFERENCE 10                 Red Wi… 41-31-… 92    82    39    2    
#> # ℹ 118 more rows
#> # ℹ 15 more variables: sol <chr>, gf <chr>, ga <chr>, gd <chr>, home <chr>,
#> #   away <chr>, l10 <chr>, strk <chr>, entity_id <chr>,
#> #   western_conference <chr>, east_atlantic <chr>, east_metropolitan <chr>,
#> #   west_central <chr>, west_pacific <chr>, wild_card <chr>
```
