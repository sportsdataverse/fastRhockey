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
#> ── Fox Sports NHL team_stats ───────────────────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 12:05:26 UTC
#> # A tibble: 17 × 6
#>    team_id category     stat                      stat_abbreviation player value
#>    <chr>   <chr>        <chr>                     <chr>             <chr>  <chr>
#>  1 1       PLAYER STATS Goals                     G                 David… 4    
#>  2 1       PLAYER STATS Points                    P                 David… 12   
#>  3 1       PLAYER STATS Plus/Minus                +/-               Hampu… 9    
#>  4 1       PLAYER STATS Shots On Goal             S                 David… 34   
#>  5 1       PLAYER STATS Takeaways                 TA                Hampu… 4    
#>  6 1       PLAYER STATS Goals Against Average     GAA               Jerem… 1.85 
#>  7 1       PLAYER STATS Shutouts                  SO                Jerem… 1    
#>  8 1       PLAYER STATS Time On Ice Per Game      TOI/G             Charl… 25:50
#>  9 1       PLAYER STATS Faceoff Wins              W                 Elias… 102  
#> 10 1       PLAYER STATS Penalty Minutes           PIM               Nikit… 29   
#> 11 1       TEAM STATS   Goal Differential         DIFF              NA     15   
#> 12 1       TEAM STATS   Power Play Percentage     PCT               NA     25.0 
#> 13 1       TEAM STATS   Power Play Kill Percenta… KPCT              NA     81.3 
#> 14 1       TEAM STATS   Shorthanded Percentage    PCT               NA     0.0  
#> 15 1       TEAM STATS   Penalty Minute Different… DIFF              NA     18.0 
#> 16 1       TEAM STATS   Takeaway / Giveaway       TA/GA             NA     0.25 
#> 17 1       TEAM STATS   Faceoff Win Percentage    FPWPCT            NA     52.9 
```
