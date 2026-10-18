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
#> ── Fox Sports NHL gamelog ──────────────────────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 12:05:25 UTC
#> # A tibble: 50 × 8
#>    team_id season_type    category game_id game_date opponent stat       value
#>    <chr>   <chr>          <chr>    <chr>   <chr>     <chr>    <chr>      <chr>
#>  1 1       REGULAR SEASON overall  44688   10/17     @LA      g          4    
#>  2 1       REGULAR SEASON overall  44688   10/17     @LA      a          6.0  
#>  3 1       REGULAR SEASON overall  44688   10/17     @LA      ga         1    
#>  4 1       REGULAR SEASON overall  44688   10/17     @LA      sa         26.0 
#>  5 1       REGULAR SEASON overall  44688   10/17     @LA      sv         25.0 
#>  6 1       REGULAR SEASON overall  44688   10/17     @LA      sv_percent .962 
#>  7 1       REGULAR SEASON overall  44688   10/17     @LA      g_2        0    
#>  8 1       REGULAR SEASON overall  44688   10/17     @LA      opp        2    
#>  9 1       REGULAR SEASON overall  44688   10/17     @LA      kpct       -    
#> 10 1       REGULAR SEASON overall  44688   10/17     @LA      fpwpct     53.0 
#> # ℹ 40 more rows
```
