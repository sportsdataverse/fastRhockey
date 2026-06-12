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
#> ℹ Data updated: 2026-06-12 14:18:38 UTC
#> # A tibble: 17 × 6
#>    team_id category     stat                      stat_abbreviation player value
#>    <chr>   <chr>        <chr>                     <chr>             <chr>  <chr>
#>  1 1       PLAYER STATS Goals                     G                 David… 3    
#>  2 1       PLAYER STATS Points                    P                 David… 7    
#>  3 1       PLAYER STATS Plus/Minus                +/-               Andre… 2    
#>  4 1       PLAYER STATS Shots On Goal             S                 David… 22   
#>  5 1       PLAYER STATS Takeaways                 TA                Mark … 5    
#>  6 1       PLAYER STATS Goals Against Average     GAA               Jerem… 2.91 
#>  7 1       PLAYER STATS Shutouts                  SO                Joona… 0    
#>  8 1       PLAYER STATS Time On Ice Per Game      TOI/G             Charl… 25:37
#>  9 1       PLAYER STATS Faceoff Wins              W                 Elias… 53   
#> 10 1       PLAYER STATS Penalty Minutes           PIM               Nikit… 37   
#> 11 1       TEAM STATS   Goal Differential         DIFF              NA     -8   
#> 12 1       TEAM STATS   Power Play Percentage     PCT               NA     12.5 
#> 13 1       TEAM STATS   Power Play Kill Percenta… KPCT              NA     95.8 
#> 14 1       TEAM STATS   Shorthanded Percentage    PCT               NA     4.2  
#> 15 1       TEAM STATS   Penalty Minute Different… DIFF              NA     30.0 
#> 16 1       TEAM STATS   Takeaway / Giveaway       TA/GA             NA     0.27 
#> 17 1       TEAM STATS   Faceoff Win Percentage    FPWPCT            NA     56.2 
```
