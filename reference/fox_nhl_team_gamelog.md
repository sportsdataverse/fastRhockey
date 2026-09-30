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
#> ℹ Data updated: 2026-09-30 14:43:05 UTC
#> # A tibble: 50 × 8
#>    team_id season_type    category game_id game_date opponent stat       value
#>    <chr>   <chr>          <chr>    <chr>   <chr>     <chr>    <chr>      <chr>
#>  1 1       REGULAR SEASON overall  44559   9/29      NYR      g          3    
#>  2 1       REGULAR SEASON overall  44559   9/29      NYR      a          5.0  
#>  3 1       REGULAR SEASON overall  44559   9/29      NYR      ga         0    
#>  4 1       REGULAR SEASON overall  44559   9/29      NYR      sa         24.0 
#>  5 1       REGULAR SEASON overall  44559   9/29      NYR      sv         24.0 
#>  6 1       REGULAR SEASON overall  44559   9/29      NYR      sv_percent 1.000
#>  7 1       REGULAR SEASON overall  44559   9/29      NYR      g_2        0    
#>  8 1       REGULAR SEASON overall  44559   9/29      NYR      opp        1    
#>  9 1       REGULAR SEASON overall  44559   9/29      NYR      kpct       -    
#> 10 1       REGULAR SEASON overall  44559   9/29      NYR      fpwpct     50.0 
#> # ℹ 40 more rows
```
