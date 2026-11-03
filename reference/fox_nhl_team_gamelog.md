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
#> ℹ Data updated: 2026-11-03 11:53:16 UTC
#> # A tibble: 50 × 8
#>    team_id season_type    category game_id game_date opponent stat       value
#>    <chr>   <chr>          <chr>    <chr>   <chr>     <chr>    <chr>      <chr>
#>  1 1       REGULAR SEASON overall  44788   11/2      VGK      g          2    
#>  2 1       REGULAR SEASON overall  44788   11/2      VGK      a          4.0  
#>  3 1       REGULAR SEASON overall  44788   11/2      VGK      ga         2    
#>  4 1       REGULAR SEASON overall  44788   11/2      VGK      sa         23.0 
#>  5 1       REGULAR SEASON overall  44788   11/2      VGK      sv         21.0 
#>  6 1       REGULAR SEASON overall  44788   11/2      VGK      sv_percent .913 
#>  7 1       REGULAR SEASON overall  44788   11/2      VGK      g_2        1    
#>  8 1       REGULAR SEASON overall  44788   11/2      VGK      opp        4    
#>  9 1       REGULAR SEASON overall  44788   11/2      VGK      kpct       -    
#> 10 1       REGULAR SEASON overall  44788   11/2      VGK      fpwpct     53.7 
#> # ℹ 40 more rows
```
