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
#> ℹ Data updated: 2026-09-30 14:43:06 UTC
#> # A tibble: 16 × 6
#>    team_id category     stat                      stat_abbreviation player value
#>    <chr>   <chr>        <chr>                     <chr>             <chr>  <chr>
#>  1 1       PLAYER STATS Goals                     G                 Mark … 1    
#>  2 1       PLAYER STATS Points                    P                 Frase… 2    
#>  3 1       PLAYER STATS Plus/Minus                +/-               Hampu… 2    
#>  4 1       PLAYER STATS Shots On Goal             S                 David… 4    
#>  5 1       PLAYER STATS Takeaways                 TA                Morga… 1    
#>  6 1       PLAYER STATS Shutouts                  SO                Jerem… 1    
#>  7 1       PLAYER STATS Time On Ice Per Game      TOI/G             Conno… 23:23
#>  8 1       PLAYER STATS Faceoff Wins              W                 Pavel… 10   
#>  9 1       PLAYER STATS Penalty Minutes           PIM               Tanne… 2    
#> 10 1       TEAM STATS   Goal Differential         DIFF              NA     3    
#> 11 1       TEAM STATS   Power Play Percentage     PCT               NA     0.0  
#> 12 1       TEAM STATS   Power Play Kill Percenta… KPCT              NA     100.0
#> 13 1       TEAM STATS   Shorthanded Percentage    PCT               NA     0.0  
#> 14 1       TEAM STATS   Penalty Minute Different… DIFF              NA     0.0  
#> 15 1       TEAM STATS   Takeaway / Giveaway       TA/GA             NA     0.40 
#> 16 1       TEAM STATS   Faceoff Win Percentage    FPWPCT            NA     50.0 
```
