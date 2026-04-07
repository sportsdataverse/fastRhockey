# **NHL Conference Info**

Returns teams belonging to a given conference, derived from standings
data.

The original NHL Stats API conferences endpoint is no longer available.
This function now extracts conference information from the standings
endpoint at `api-web.nhle.com`.

## Usage

``` r
nhl_conferences_info(conference_name, date = NULL)
```

## Arguments

- conference_name:

  Character. Conference name (e.g. "Eastern" or "Western").

- date:

  Character date in "YYYY-MM-DD" format. If NULL, returns current
  conference info.

## Value

Returns a data frame of teams in the specified conference with columns
from
[`nhl_standings`](https://fastRhockey.sportsdataverse.org/reference/nhl_standings.md)
filtered by conference.

## Examples

``` r
# \donttest{
  try(nhl_conferences_info(conference_name = "Eastern"))
#> ── NHL Conference Information from NHL.com ──────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-07 16:23:14 UTC
#> # A tibble: 16 × 36
#>    team_abbr team_name             team_common_name team_logo    conference_name
#>    <chr>     <chr>                 <chr>            <chr>        <chr>          
#>  1 CAR       Carolina Hurricanes   Hurricanes       https://ass… Eastern        
#>  2 TBL       Tampa Bay Lightning   Lightning        https://ass… Eastern        
#>  3 BUF       Buffalo Sabres        Sabres           https://ass… Eastern        
#>  4 MTL       Montréal Canadiens    Canadiens        https://ass… Eastern        
#>  5 PIT       Pittsburgh Penguins   Penguins         https://ass… Eastern        
#>  6 BOS       Boston Bruins         Bruins           https://ass… Eastern        
#>  7 OTT       Ottawa Senators       Senators         https://ass… Eastern        
#>  8 PHI       Philadelphia Flyers   Flyers           https://ass… Eastern        
#>  9 NYI       New York Islanders    Islanders        https://ass… Eastern        
#> 10 DET       Detroit Red Wings     Red Wings        https://ass… Eastern        
#> 11 CBJ       Columbus Blue Jackets Blue Jackets     https://ass… Eastern        
#> 12 WSH       Washington Capitals   Capitals         https://ass… Eastern        
#> 13 NJD       New Jersey Devils     Devils           https://ass… Eastern        
#> 14 TOR       Toronto Maple Leafs   Maple Leafs      https://ass… Eastern        
#> 15 FLA       Florida Panthers      Panthers         https://ass… Eastern        
#> 16 NYR       New York Rangers      Rangers          https://ass… Eastern        
#> # ℹ 31 more variables: division_abbrev <chr>, division_name <chr>,
#> #   place_name <chr>, conference_sequence <int>, division_sequence <int>,
#> #   league_sequence <int>, wildcard_sequence <int>, games_played <int>,
#> #   wins <int>, losses <int>, ot_losses <int>, points <int>, point_pctg <dbl>,
#> #   regulation_wins <int>, regulation_plus_ot_wins <int>, goals_for <int>,
#> #   goals_against <int>, goal_differential <int>, home_wins <int>,
#> #   home_losses <int>, home_ot_losses <int>, road_wins <int>, …
# }
```
