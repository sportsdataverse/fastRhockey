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
#> ℹ Data updated: 2026-11-26 22:25:29 UTC
#> # A tibble: 50 × 8
#>    team_id season_type    category game_id game_date opponent stat       value
#>    <chr>   <chr>          <chr>    <chr>   <chr>     <chr>    <chr>      <chr>
#>  1 1       REGULAR SEASON overall  44942   11/25     WPG      g          2    
#>  2 1       REGULAR SEASON overall  44942   11/25     WPG      a          4.0  
#>  3 1       REGULAR SEASON overall  44942   11/25     WPG      ga         4    
#>  4 1       REGULAR SEASON overall  44942   11/25     WPG      sa         18.0 
#>  5 1       REGULAR SEASON overall  44942   11/25     WPG      sv         14.0 
#>  6 1       REGULAR SEASON overall  44942   11/25     WPG      sv_percent .778 
#>  7 1       REGULAR SEASON overall  44942   11/25     WPG      g_2        1    
#>  8 1       REGULAR SEASON overall  44942   11/25     WPG      opp        3    
#>  9 1       REGULAR SEASON overall  44942   11/25     WPG      kpct       -    
#> 10 1       REGULAR SEASON overall  44942   11/25     WPG      fpwpct     65.4 
#> # ℹ 40 more rows
```
