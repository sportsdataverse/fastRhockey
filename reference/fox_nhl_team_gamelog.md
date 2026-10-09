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
#> ℹ Data updated: 2026-10-09 03:19:36 UTC
#> # A tibble: 50 × 8
#>    team_id season_type    category game_id game_date opponent stat       value
#>    <chr>   <chr>          <chr>    <chr>   <chr>     <chr>    <chr>      <chr>
#>  1 1       REGULAR SEASON overall  44615   10/8      UTA      g          6    
#>  2 1       REGULAR SEASON overall  44615   10/8      UTA      a          11.0 
#>  3 1       REGULAR SEASON overall  44615   10/8      UTA      ga         1    
#>  4 1       REGULAR SEASON overall  44615   10/8      UTA      sa         37.0 
#>  5 1       REGULAR SEASON overall  44615   10/8      UTA      sv         36.0 
#>  6 1       REGULAR SEASON overall  44615   10/8      UTA      sv_percent .973 
#>  7 1       REGULAR SEASON overall  44615   10/8      UTA      g_2        3    
#>  8 1       REGULAR SEASON overall  44615   10/8      UTA      opp        4    
#>  9 1       REGULAR SEASON overall  44615   10/8      UTA      kpct       -    
#> 10 1       REGULAR SEASON overall  44615   10/8      UTA      fpwpct     46.4 
#> # ℹ 40 more rows
```
