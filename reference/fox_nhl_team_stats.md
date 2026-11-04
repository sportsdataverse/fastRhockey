# **Get Fox Sports NHL team stat leaders**

**Get Fox Sports NHL team stat leaders**

## Usage

``` r
fox_nhl_team_stats(team_id)
```

## Arguments

- team_id:

  Fox Bifrost team id.

## Value

A `fastRhockey_data` tibble (`team_id`, `category`, `stat`,
`stat_abbreviation`, `player`, `value`).

## Examples

``` r
 try(fox_nhl_team_stats("1")) 
#> ── Fox Sports NHL team_stats ────────────────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-04 04:38:52 UTC
#> # A tibble: 17 × 6
#>    team_id category     stat                      stat_abbreviation player value
#>    <chr>   <chr>        <chr>                     <chr>             <chr>  <chr>
#>  1 1       PLAYER STATS Goals                     G                 David… 8    
#>  2 1       PLAYER STATS Points                    P                 David… 20   
#>  3 1       PLAYER STATS Plus/Minus                +/-               Willi… 16   
#>  4 1       PLAYER STATS Shots On Goal             S                 David… 57   
#>  5 1       PLAYER STATS Takeaways                 TA                Mason… 9    
#>  6 1       PLAYER STATS Goals Against Average     GAA               Micha… 2.26 
#>  7 1       PLAYER STATS Shutouts                  SO                Micha… 1    
#>  8 1       PLAYER STATS Time On Ice Per Game      TOI/G             Charl… 25:10
#>  9 1       PLAYER STATS Faceoff Wins              W                 Elias… 187  
#> 10 1       PLAYER STATS Penalty Minutes           PIM               Nikit… 47   
#> 11 1       TEAM STATS   Goal Differential         DIFF              NA     19   
#> 12 1       TEAM STATS   Power Play Percentage     PCT               NA     20.0 
#> 13 1       TEAM STATS   Power Play Kill Percenta… KPCT              NA     76.4 
#> 14 1       TEAM STATS   Shorthanded Percentage    PCT               NA     0.0  
#> 15 1       TEAM STATS   Penalty Minute Different… DIFF              NA     11.0 
#> 16 1       TEAM STATS   Takeaway / Giveaway       TA/GA             NA     0.24 
#> 17 1       TEAM STATS   Faceoff Win Percentage    FPWPCT            NA     54.6 
```
