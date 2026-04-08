# **NHL Divisions Info**

Returns teams belonging to a given division, derived from standings
data.

The original NHL Stats API divisions endpoint is no longer available.
This function now extracts division information from the standings
endpoint at `api-web.nhle.com`.

## Usage

``` r
nhl_divisions_info(division_name, date = NULL)
```

## Arguments

- division_name:

  Character. Division name (e.g. "Atlantic", "Metropolitan", "Central",
  "Pacific").

- date:

  Character date in "YYYY-MM-DD" format. If NULL, returns current
  division info.

## Value

Returns a data frame of teams in the specified division with columns
from
[`nhl_standings`](https://fastRhockey.sportsdataverse.org/reference/nhl_standings.md)
filtered by division.

## Examples

``` r
# \donttest{
  try(nhl_divisions_info(division_name = "Atlantic"))
#> ── NHL Division Information from NHL.com ────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-08 05:46:36 UTC
#> # A tibble: 8 × 36
#>   team_abbr team_name team_common_name team_logo conference_name division_abbrev
#>   <chr>     <chr>     <chr>            <chr>     <chr>           <chr>          
#> 1 TBL       Tampa Ba… Lightning        https://… Eastern         A              
#> 2 BUF       Buffalo … Sabres           https://… Eastern         A              
#> 3 MTL       Montréal… Canadiens        https://… Eastern         A              
#> 4 BOS       Boston B… Bruins           https://… Eastern         A              
#> 5 OTT       Ottawa S… Senators         https://… Eastern         A              
#> 6 DET       Detroit … Red Wings        https://… Eastern         A              
#> 7 TOR       Toronto … Maple Leafs      https://… Eastern         A              
#> 8 FLA       Florida … Panthers         https://… Eastern         A              
#> # ℹ 30 more variables: division_name <chr>, place_name <chr>,
#> #   conference_sequence <int>, division_sequence <int>, league_sequence <int>,
#> #   wildcard_sequence <int>, games_played <int>, wins <int>, losses <int>,
#> #   ot_losses <int>, points <int>, point_pctg <dbl>, regulation_wins <int>,
#> #   regulation_plus_ot_wins <int>, goals_for <int>, goals_against <int>,
#> #   goal_differential <int>, home_wins <int>, home_losses <int>,
#> #   home_ot_losses <int>, road_wins <int>, road_losses <int>, …
# }
```
