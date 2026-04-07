# **NHL Standings**

Returns current or historical NHL standings. Uses the new NHL API
(`api-web.nhle.com`).

## Usage

``` r
nhl_standings(date = NULL)
```

## Arguments

- date:

  Character date in "YYYY-MM-DD" format. If NULL, returns current
  standings.

## Value

Returns a data frame with standings information.

## Examples

``` r
# \donttest{
  try(nhl_standings())
#> ── NHL Standings ────────────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-07 07:09:30 UTC
#> # A tibble: 32 × 36
#>    team_abbr team_name           team_common_name team_logo      conference_name
#>    <chr>     <chr>               <chr>            <chr>          <chr>          
#>  1 COL       Colorado Avalanche  Avalanche        https://asset… Western        
#>  2 CAR       Carolina Hurricanes Hurricanes       https://asset… Eastern        
#>  3 TBL       Tampa Bay Lightning Lightning        https://asset… Eastern        
#>  4 DAL       Dallas Stars        Stars            https://asset… Western        
#>  5 BUF       Buffalo Sabres      Sabres           https://asset… Eastern        
#>  6 MTL       Montréal Canadiens  Canadiens        https://asset… Eastern        
#>  7 MIN       Minnesota Wild      Wild             https://asset… Western        
#>  8 PIT       Pittsburgh Penguins Penguins         https://asset… Eastern        
#>  9 BOS       Boston Bruins       Bruins           https://asset… Eastern        
#> 10 OTT       Ottawa Senators     Senators         https://asset… Eastern        
#> # ℹ 22 more rows
#> # ℹ 31 more variables: division_abbrev <chr>, division_name <chr>,
#> #   place_name <chr>, conference_sequence <int>, division_sequence <int>,
#> #   league_sequence <int>, wildcard_sequence <int>, games_played <int>,
#> #   wins <int>, losses <int>, ot_losses <int>, points <int>, point_pctg <dbl>,
#> #   regulation_wins <int>, regulation_plus_ot_wins <int>, goals_for <int>,
#> #   goals_against <int>, goal_differential <int>, home_wins <int>, …
  try(nhl_standings(date = "2024-03-01"))
#> ── NHL Standings ────────────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-07 07:09:31 UTC
#> # A tibble: 32 × 36
#>    team_abbr team_name            team_common_name team_logo     conference_name
#>    <chr>     <chr>                <chr>            <chr>         <chr>          
#>  1 FLA       Florida Panthers     Panthers         https://asse… Eastern        
#>  2 BOS       Boston Bruins        Bruins           https://asse… Eastern        
#>  3 NYR       New York Rangers     Rangers          https://asse… Eastern        
#>  4 VAN       Vancouver Canucks    Canucks          https://asse… Western        
#>  5 DAL       Dallas Stars         Stars            https://asse… Western        
#>  6 WPG       Winnipeg Jets        Jets             https://asse… Western        
#>  7 COL       Colorado Avalanche   Avalanche        https://asse… Western        
#>  8 CAR       Carolina Hurricanes  Hurricanes       https://asse… Eastern        
#>  9 TOR       Toronto Maple Leafs  Maple Leafs      https://asse… Eastern        
#> 10 VGK       Vegas Golden Knights Golden Knights   https://asse… Western        
#> # ℹ 22 more rows
#> # ℹ 31 more variables: division_abbrev <chr>, division_name <chr>,
#> #   place_name <chr>, conference_sequence <int>, division_sequence <int>,
#> #   league_sequence <int>, wildcard_sequence <int>, games_played <int>,
#> #   wins <int>, losses <int>, ot_losses <int>, points <int>, point_pctg <dbl>,
#> #   regulation_wins <int>, regulation_plus_ot_wins <int>, goals_for <int>,
#> #   goals_against <int>, goal_differential <int>, home_wins <int>, …
# }
```
