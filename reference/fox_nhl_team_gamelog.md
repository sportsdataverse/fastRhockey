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
#> ℹ Data updated: 2026-11-02 03:48:13 UTC
#> # A tibble: 50 × 8
#>    team_id season_type    category game_id game_date opponent stat       value
#>    <chr>   <chr>          <chr>    <chr>   <chr>     <chr>    <chr>      <chr>
#>  1 1       REGULAR SEASON overall  44772   10/31     CHI      g          5    
#>  2 1       REGULAR SEASON overall  44772   10/31     CHI      a          7.0  
#>  3 1       REGULAR SEASON overall  44772   10/31     CHI      ga         4    
#>  4 1       REGULAR SEASON overall  44772   10/31     CHI      sa         41.0 
#>  5 1       REGULAR SEASON overall  44772   10/31     CHI      sv         37.0 
#>  6 1       REGULAR SEASON overall  44772   10/31     CHI      sv_percent .902 
#>  7 1       REGULAR SEASON overall  44772   10/31     CHI      g_2        0    
#>  8 1       REGULAR SEASON overall  44772   10/31     CHI      opp        3    
#>  9 1       REGULAR SEASON overall  44772   10/31     CHI      kpct       -    
#> 10 1       REGULAR SEASON overall  44772   10/31     CHI      fpwpct     69.4 
#> # ℹ 40 more rows
```
