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
#> ── Fox Sports NHL gamelog ───────────────────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-25 06:29:50 UTC
#> # A tibble: 50 × 8
#>    team_id season_type    category game_id game_date opponent stat       value
#>    <chr>   <chr>          <chr>    <chr>   <chr>     <chr>    <chr>      <chr>
#>  1 1       REGULAR SEASON overall  44918   11/21     WSH      g          7    
#>  2 1       REGULAR SEASON overall  44918   11/21     WSH      a          13.0 
#>  3 1       REGULAR SEASON overall  44918   11/21     WSH      ga         5    
#>  4 1       REGULAR SEASON overall  44918   11/21     WSH      sa         36.0 
#>  5 1       REGULAR SEASON overall  44918   11/21     WSH      sv         31.0 
#>  6 1       REGULAR SEASON overall  44918   11/21     WSH      sv_percent .861 
#>  7 1       REGULAR SEASON overall  44918   11/21     WSH      g_2        1    
#>  8 1       REGULAR SEASON overall  44918   11/21     WSH      opp        4    
#>  9 1       REGULAR SEASON overall  44918   11/21     WSH      kpct       -    
#> 10 1       REGULAR SEASON overall  44918   11/21     WSH      fpwpct     40.6 
#> # ℹ 40 more rows
```
