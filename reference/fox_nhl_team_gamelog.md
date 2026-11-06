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
#> ℹ Data updated: 2026-11-06 07:46:43 UTC
#> # A tibble: 50 × 8
#>    team_id season_type    category game_id game_date opponent stat       value
#>    <chr>   <chr>          <chr>    <chr>   <chr>     <chr>    <chr>      <chr>
#>  1 1       REGULAR SEASON overall  44808   11/5      @PIT     g          6    
#>  2 1       REGULAR SEASON overall  44808   11/5      @PIT     a          8.0  
#>  3 1       REGULAR SEASON overall  44808   11/5      @PIT     ga         2    
#>  4 1       REGULAR SEASON overall  44808   11/5      @PIT     sa         18.0 
#>  5 1       REGULAR SEASON overall  44808   11/5      @PIT     sv         16.0 
#>  6 1       REGULAR SEASON overall  44808   11/5      @PIT     sv_percent .889 
#>  7 1       REGULAR SEASON overall  44808   11/5      @PIT     g_2        0    
#>  8 1       REGULAR SEASON overall  44808   11/5      @PIT     opp        1    
#>  9 1       REGULAR SEASON overall  44808   11/5      @PIT     kpct       -    
#> 10 1       REGULAR SEASON overall  44808   11/5      @PIT     fpwpct     62.0 
#> # ℹ 40 more rows
```
