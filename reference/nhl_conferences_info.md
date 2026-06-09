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

A data frame (`fastRhockey_data`) with the following columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| team_abbr | character | Team abbreviation. |
| team_name | character | Team name. |
| team_common_name | character | Team common (nickname) name. |
| team_logo | character | URL to the team logo image. |
| conference_name | character | Conference name. |
| division_abbrev | character | Division abbreviation. |
| division_name | character | Division name. |
| place_name | character | Team place (city) name. |
| conference_sequence | integer | Team's seeding position within the conference. |
| division_sequence | integer | Team's seeding position within the division. |
| league_sequence | integer | Team's seeding position within the league. |
| wildcard_sequence | integer | Team's wild card seeding position. |
| games_played | integer | Games played. |
| wins | integer | Wins. |
| losses | integer | Losses. |
| ot_losses | integer | Overtime losses. |
| points | integer | Standings points. |
| point_pctg | numeric | Points percentage. |
| regulation_wins | integer | Wins in regulation. |
| regulation_plus_ot_wins | integer | Wins in regulation plus overtime. |
| goals_for | integer | Goals scored. |
| goals_against | integer | Goals against. |
| goal_differential | integer | Goal differential (goals for minus goals against). |
| home_wins | integer | Home wins. |
| home_losses | integer | Home losses. |
| home_ot_losses | integer | Home overtime losses. |
| road_wins | integer | Road wins. |
| road_losses | integer | Road losses. |
| road_ot_losses | integer | Road overtime losses. |
| l10_wins | integer | Wins in the last ten games. |
| l10_losses | integer | Losses in the last ten games. |
| l10_ot_losses | integer | Overtime losses in the last ten games. |
| streak_code | character | Current streak type code (W/L/OT). |
| streak_count | integer | Length of the current streak. |
| shootout_wins | integer | Shootout wins. |
| shootout_losses | integer | Shootout losses. |

## Examples

``` r
# \donttest{
  try(nhl_conferences_info(conference_name = "Eastern"))
#> ── NHL Conference Information from NHL.com ──────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-06-09 20:28:32 UTC
#> # A tibble: 16 × 36
#>    team_abbr team_name             team_common_name team_logo    conference_name
#>    <chr>     <chr>                 <chr>            <chr>        <chr>          
#>  1 CAR       Carolina Hurricanes   Hurricanes       https://ass… Eastern        
#>  2 BUF       Buffalo Sabres        Sabres           https://ass… Eastern        
#>  3 TBL       Tampa Bay Lightning   Lightning        https://ass… Eastern        
#>  4 MTL       Montréal Canadiens    Canadiens        https://ass… Eastern        
#>  5 BOS       Boston Bruins         Bruins           https://ass… Eastern        
#>  6 OTT       Ottawa Senators       Senators         https://ass… Eastern        
#>  7 PIT       Pittsburgh Penguins   Penguins         https://ass… Eastern        
#>  8 PHI       Philadelphia Flyers   Flyers           https://ass… Eastern        
#>  9 WSH       Washington Capitals   Capitals         https://ass… Eastern        
#> 10 DET       Detroit Red Wings     Red Wings        https://ass… Eastern        
#> 11 CBJ       Columbus Blue Jackets Blue Jackets     https://ass… Eastern        
#> 12 NYI       New York Islanders    Islanders        https://ass… Eastern        
#> 13 NJD       New Jersey Devils     Devils           https://ass… Eastern        
#> 14 FLA       Florida Panthers      Panthers         https://ass… Eastern        
#> 15 TOR       Toronto Maple Leafs   Maple Leafs      https://ass… Eastern        
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
