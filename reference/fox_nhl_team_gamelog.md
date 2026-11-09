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
#> ℹ Data updated: 2026-11-09 11:50:20 UTC
#> # A tibble: 50 × 8
#>    team_id season_type    category game_id game_date opponent stat       value
#>    <chr>   <chr>          <chr>    <chr>   <chr>     <chr>    <chr>      <chr>
#>  1 1       REGULAR SEASON overall  44832   11/8      FLA      g          4    
#>  2 1       REGULAR SEASON overall  44832   11/8      FLA      a          7.0  
#>  3 1       REGULAR SEASON overall  44832   11/8      FLA      ga         1    
#>  4 1       REGULAR SEASON overall  44832   11/8      FLA      sa         41.0 
#>  5 1       REGULAR SEASON overall  44832   11/8      FLA      sv         40.0 
#>  6 1       REGULAR SEASON overall  44832   11/8      FLA      sv_percent .976 
#>  7 1       REGULAR SEASON overall  44832   11/8      FLA      g_2        1    
#>  8 1       REGULAR SEASON overall  44832   11/8      FLA      opp        9    
#>  9 1       REGULAR SEASON overall  44832   11/8      FLA      kpct       -    
#> 10 1       REGULAR SEASON overall  44832   11/8      FLA      fpwpct     63.6 
#> # ℹ 40 more rows
```
