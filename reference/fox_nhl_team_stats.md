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
#> ℹ Data updated: 2026-07-12 18:40:24 UTC
#> # A tibble: 17 × 6
#>    team_id category     stat                      stat_abbreviation player value
#>    <chr>   <chr>        <chr>                     <chr>             <chr>  <chr>
#>  1 1       PLAYER STATS Goals                     G                 Morga… 39   
#>  2 1       PLAYER STATS Points                    P                 David… 100  
#>  3 1       PLAYER STATS Plus/Minus                +/-               Jonat… 30   
#>  4 1       PLAYER STATS Shots On Goal             S                 David… 261  
#>  5 1       PLAYER STATS Takeaways                 TA                Nikit… 33   
#>  6 1       PLAYER STATS Goals Against Average     GAA               Jerem… 2.71 
#>  7 1       PLAYER STATS Shutouts                  SO                Jerem… 2    
#>  8 1       PLAYER STATS Time On Ice Per Game      TOI/G             Charl… 24:23
#>  9 1       PLAYER STATS Faceoff Wins              W                 Elias… 660  
#> 10 1       PLAYER STATS Penalty Minutes           PIM               Nikit… 152  
#> 11 1       TEAM STATS   Goal Differential         DIFF              NA     21   
#> 12 1       TEAM STATS   Power Play Percentage     PCT               NA     23.4 
#> 13 1       TEAM STATS   Power Play Kill Percenta… KPCT              NA     76.9 
#> 14 1       TEAM STATS   Shorthanded Percentage    PCT               NA     0.7  
#> 15 1       TEAM STATS   Penalty Minute Different… DIFF              NA     117.0
#> 16 1       TEAM STATS   Takeaway / Giveaway       TA/GA             NA     0.28 
#> 17 1       TEAM STATS   Faceoff Win Percentage    FPWPCT            NA     53.1 
```
