# **Get Fox Sports NHL team game log**

**Get Fox Sports NHL team game log**

## Usage

``` r
fox_nhl_team_gamelog(team_id)
```

## Arguments

- team_id:

  Fox Bifrost team id.

## Value

A `fastRhockey_data` tibble (long): `team_id`, `season_type`,
`category`, `game_id`, `game_date`, `opponent`, `stat`, `value`.

## Examples

``` r
 try(fox_nhl_team_gamelog("1")) 
#> ── Fox Sports NHL gamelog ───────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-10-11 16:01:37 UTC
#> # A tibble: 50 × 8
#>    team_id season_type    category game_id game_date opponent stat       value
#>    <chr>   <chr>          <chr>    <chr>   <chr>     <chr>    <chr>      <chr>
#>  1 1       REGULAR SEASON overall  44626   10/10     PHI      g          5    
#>  2 1       REGULAR SEASON overall  44626   10/10     PHI      a          9.0  
#>  3 1       REGULAR SEASON overall  44626   10/10     PHI      ga         1    
#>  4 1       REGULAR SEASON overall  44626   10/10     PHI      sa         15.0 
#>  5 1       REGULAR SEASON overall  44626   10/10     PHI      sv         14.0 
#>  6 1       REGULAR SEASON overall  44626   10/10     PHI      sv_percent .933 
#>  7 1       REGULAR SEASON overall  44626   10/10     PHI      g_2        0    
#>  8 1       REGULAR SEASON overall  44626   10/10     PHI      opp        2    
#>  9 1       REGULAR SEASON overall  44626   10/10     PHI      kpct       -    
#> 10 1       REGULAR SEASON overall  44626   10/10     PHI      fpwpct     55.1 
#> # ℹ 40 more rows
```
