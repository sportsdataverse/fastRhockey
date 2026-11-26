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
#> ℹ Data updated: 2026-11-26 22:25:29 UTC
#> # A tibble: 17 × 6
#>    team_id category     stat                      stat_abbreviation player value
#>    <chr>   <chr>        <chr>                     <chr>             <chr>  <chr>
#>  1 1       PLAYER STATS Goals                     G                 Elias… 11   
#>  2 1       PLAYER STATS Points                    P                 David… 32   
#>  3 1       PLAYER STATS Plus/Minus                +/-               Willi… 16   
#>  4 1       PLAYER STATS Shots On Goal             S                 David… 88   
#>  5 1       PLAYER STATS Takeaways                 TA                Hampu… 9    
#>  6 1       PLAYER STATS Goals Against Average     GAA               Micha… 2.15 
#>  7 1       PLAYER STATS Shutouts                  SO                Micha… 1    
#>  8 1       PLAYER STATS Time On Ice Per Game      TOI/G             Charl… 24:17
#>  9 1       PLAYER STATS Faceoff Wins              W                 Elias… 290  
#> 10 1       PLAYER STATS Penalty Minutes           PIM               Nikit… 80   
#> 11 1       TEAM STATS   Goal Differential         DIFF              NA     19   
#> 12 1       TEAM STATS   Power Play Percentage     PCT               NA     20.0 
#> 13 1       TEAM STATS   Power Play Kill Percenta… KPCT              NA     80.0 
#> 14 1       TEAM STATS   Shorthanded Percentage    PCT               NA     0.0  
#> 15 1       TEAM STATS   Penalty Minute Different… DIFF              NA     11.0 
#> 16 1       TEAM STATS   Takeaway / Giveaway       TA/GA             NA     0.22 
#> 17 1       TEAM STATS   Faceoff Win Percentage    FPWPCT            NA     55.7 
```
