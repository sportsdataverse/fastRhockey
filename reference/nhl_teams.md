# **NHL Teams**

Returns current NHL team information. Uses the NHL API standings
endpoint to get up-to-date team info.

## Usage

``` r
nhl_teams(season = NULL)
```

## Arguments

- season:

  Integer four-digit year (e.g., 2024). If NULL, returns current teams.

## Value

Returns a data frame with team information.

## Examples

``` r
# \donttest{
  try(nhl_teams())
#> ── NHL Teams ────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-08 07:22:00 UTC
#> # A tibble: 32 × 20
#>    team_abbr team_name           team_common_name team_logo      conference_abbr
#>    <chr>     <chr>               <chr>            <chr>          <chr>          
#>  1 COL       Colorado Avalanche  Avalanche        https://asset… Western        
#>  2 CAR       Carolina Hurricanes Hurricanes       https://asset… Eastern        
#>  3 DAL       Dallas Stars        Stars            https://asset… Western        
#>  4 TBL       Tampa Bay Lightning Lightning        https://asset… Eastern        
#>  5 BUF       Buffalo Sabres      Sabres           https://asset… Eastern        
#>  6 MTL       Montréal Canadiens  Canadiens        https://asset… Eastern        
#>  7 MIN       Minnesota Wild      Wild             https://asset… Western        
#>  8 PIT       Pittsburgh Penguins Penguins         https://asset… Eastern        
#>  9 BOS       Boston Bruins       Bruins           https://asset… Eastern        
#> 10 OTT       Ottawa Senators     Senators         https://asset… Eastern        
#> # ℹ 22 more rows
#> # ℹ 15 more variables: conference_name <chr>, division_abbr <chr>,
#> #   division_name <chr>, place_name <chr>, games_played <int>, wins <int>,
#> #   losses <int>, ot_losses <int>, points <int>, point_pctg <dbl>,
#> #   goals_for <int>, goals_against <int>, goal_differential <int>,
#> #   streak_code <chr>, streak_count <int>
# }
```
