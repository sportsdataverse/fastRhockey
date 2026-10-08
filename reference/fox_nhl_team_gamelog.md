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
#> ℹ Data updated: 2026-10-08 08:18:25 UTC
#> # A tibble: 50 × 8
#>    team_id season_type    category game_id game_date opponent stat       value
#>    <chr>   <chr>          <chr>    <chr>   <chr>     <chr>    <chr>      <chr>
#>  1 1       REGULAR SEASON overall  44598   10/5      OTT      g          1    
#>  2 1       REGULAR SEASON overall  44598   10/5      OTT      a          0.0  
#>  3 1       REGULAR SEASON overall  44598   10/5      OTT      ga         4    
#>  4 1       REGULAR SEASON overall  44598   10/5      OTT      sa         24.0 
#>  5 1       REGULAR SEASON overall  44598   10/5      OTT      sv         20.0 
#>  6 1       REGULAR SEASON overall  44598   10/5      OTT      sv_percent .833 
#>  7 1       REGULAR SEASON overall  44598   10/5      OTT      g_2        0    
#>  8 1       REGULAR SEASON overall  44598   10/5      OTT      opp        4    
#>  9 1       REGULAR SEASON overall  44598   10/5      OTT      kpct       -    
#> 10 1       REGULAR SEASON overall  44598   10/5      OTT      fpwpct     51.6 
#> # ℹ 40 more rows
```
