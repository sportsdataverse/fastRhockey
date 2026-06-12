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
#> ℹ Data updated: 2026-06-12 13:45:19 UTC
#> # A tibble: 50 × 8
#>    team_id season_type category game_id game_date opponent stat       value
#>    <chr>   <chr>       <chr>    <chr>   <chr>     <chr>    <chr>      <chr>
#>  1 1       POSTSEASON  overall  44421   5/1       BUF      g          1    
#>  2 1       POSTSEASON  overall  44421   5/1       BUF      a          2.0  
#>  3 1       POSTSEASON  overall  44421   5/1       BUF      ga         4    
#>  4 1       POSTSEASON  overall  44421   5/1       BUF      sa         26.0 
#>  5 1       POSTSEASON  overall  44421   5/1       BUF      sv         22.0 
#>  6 1       POSTSEASON  overall  44421   5/1       BUF      sv_percent .846 
#>  7 1       POSTSEASON  overall  44421   5/1       BUF      g_2        0    
#>  8 1       POSTSEASON  overall  44421   5/1       BUF      opp        0    
#>  9 1       POSTSEASON  overall  44421   5/1       BUF      kpct       -    
#> 10 1       POSTSEASON  overall  44421   5/1       BUF      fpwpct     55.9 
#> # ℹ 40 more rows
```
