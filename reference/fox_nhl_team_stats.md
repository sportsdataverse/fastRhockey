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
#> ── Fox Sports NHL team_stats ────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-10-08 11:30:26 UTC
#> # A tibble: 17 × 6
#>    team_id category     stat                      stat_abbreviation player value
#>    <chr>   <chr>        <chr>                     <chr>             <chr>  <chr>
#>  1 1       PLAYER STATS Goals                     G                 JJ Pe… 2    
#>  2 1       PLAYER STATS Points                    P                 Mark … 3    
#>  3 1       PLAYER STATS Plus/Minus                +/-               Willi… 3    
#>  4 1       PLAYER STATS Shots On Goal             S                 David… 15   
#>  5 1       PLAYER STATS Takeaways                 TA                Casey… 3    
#>  6 1       PLAYER STATS Goals Against Average     GAA               Jerem… 2.31 
#>  7 1       PLAYER STATS Shutouts                  SO                Jerem… 1    
#>  8 1       PLAYER STATS Time On Ice Per Game      TOI/G             Mason… 20:48
#>  9 1       PLAYER STATS Faceoff Wins              W                 Elias… 41   
#> 10 1       PLAYER STATS Penalty Minutes           PIM               Nikit… 14   
#> 11 1       TEAM STATS   Goal Differential         DIFF              NA     -2   
#> 12 1       TEAM STATS   Power Play Percentage     PCT               NA     0.0  
#> 13 1       TEAM STATS   Power Play Kill Percenta… KPCT              NA     72.7 
#> 14 1       TEAM STATS   Shorthanded Percentage    PCT               NA     0.0  
#> 15 1       TEAM STATS   Penalty Minute Different… DIFF              NA     12.0 
#> 16 1       TEAM STATS   Takeaway / Giveaway       TA/GA             NA     0.27 
#> 17 1       TEAM STATS   Faceoff Win Percentage    FPWPCT            NA     50.4 
```
