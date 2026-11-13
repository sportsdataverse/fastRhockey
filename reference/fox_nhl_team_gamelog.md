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
#> ℹ Data updated: 2026-11-13 20:17:19 UTC
#> # A tibble: 50 × 8
#>    team_id season_type    category game_id game_date opponent stat       value
#>    <chr>   <chr>          <chr>    <chr>   <chr>     <chr>    <chr>      <chr>
#>  1 1       REGULAR SEASON overall  44854   11/12     MTL      g          2    
#>  2 1       REGULAR SEASON overall  44854   11/12     MTL      a          4.0  
#>  3 1       REGULAR SEASON overall  44854   11/12     MTL      ga         5    
#>  4 1       REGULAR SEASON overall  44854   11/12     MTL      sa         31.0 
#>  5 1       REGULAR SEASON overall  44854   11/12     MTL      sv         26.0 
#>  6 1       REGULAR SEASON overall  44854   11/12     MTL      sv_percent .839 
#>  7 1       REGULAR SEASON overall  44854   11/12     MTL      g_2        0    
#>  8 1       REGULAR SEASON overall  44854   11/12     MTL      opp        1    
#>  9 1       REGULAR SEASON overall  44854   11/12     MTL      kpct       -    
#> 10 1       REGULAR SEASON overall  44854   11/12     MTL      fpwpct     56.0 
#> # ℹ 40 more rows
```
