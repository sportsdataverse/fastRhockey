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
#> ── Fox Sports NHL gamelog ──────────────────────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-30 00:11:10 UTC
#> # A tibble: 50 × 8
#>    team_id season_type    category game_id game_date opponent stat       value
#>    <chr>   <chr>          <chr>    <chr>   <chr>     <chr>    <chr>      <chr>
#>  1 1       REGULAR SEASON overall  44746   10/27     @CAR     g          1    
#>  2 1       REGULAR SEASON overall  44746   10/27     @CAR     a          2.0  
#>  3 1       REGULAR SEASON overall  44746   10/27     @CAR     ga         2    
#>  4 1       REGULAR SEASON overall  44746   10/27     @CAR     sa         30.0 
#>  5 1       REGULAR SEASON overall  44746   10/27     @CAR     sv         28.0 
#>  6 1       REGULAR SEASON overall  44746   10/27     @CAR     sv_percent .933 
#>  7 1       REGULAR SEASON overall  44746   10/27     @CAR     g_2        1    
#>  8 1       REGULAR SEASON overall  44746   10/27     @CAR     opp        5    
#>  9 1       REGULAR SEASON overall  44746   10/27     @CAR     kpct       -    
#> 10 1       REGULAR SEASON overall  44746   10/27     @CAR     fpwpct     52.6 
#> # ℹ 40 more rows
```
