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
  try(nhl_divisions_info(division_name = "Atlantic"))
#> ── NHL Division Information from NHL.com ────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-07-12 18:41:28 UTC
#> # A tibble: 8 × 36
#>   team_abbr team_name team_common_name team_logo conference_name division_abbrev
#>   <chr>     <chr>     <chr>            <chr>     <chr>           <chr>          
#> 1 BUF       Buffalo … Sabres           https://… Eastern         A              
#> 2 TBL       Tampa Ba… Lightning        https://… Eastern         A              
#> 3 MTL       Montréal… Canadiens        https://… Eastern         A              
#> 4 BOS       Boston B… Bruins           https://… Eastern         A              
#> 5 OTT       Ottawa S… Senators         https://… Eastern         A              
#> 6 DET       Detroit … Red Wings        https://… Eastern         A              
#> 7 FLA       Florida … Panthers         https://… Eastern         A              
#> 8 TOR       Toronto … Maple Leafs      https://… Eastern         A              
#> # ℹ 30 more variables: division_name <chr>, place_name <chr>,
#> #   conference_sequence <int>, division_sequence <int>, league_sequence <int>,
#> #   wildcard_sequence <int>, games_played <int>, wins <int>, losses <int>,
#> #   ot_losses <int>, points <int>, point_pctg <dbl>, regulation_wins <int>,
#> #   regulation_plus_ot_wins <int>, goals_for <int>, goals_against <int>,
#> #   goal_differential <int>, home_wins <int>, home_losses <int>,
#> #   home_ot_losses <int>, road_wins <int>, road_losses <int>, …
# }
```
