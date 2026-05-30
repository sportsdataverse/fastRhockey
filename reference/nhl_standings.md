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

A data frame (`fastRhockey_data`) with the following columns:

|                         |           |                                   |
|-------------------------|-----------|-----------------------------------|
| col_name                | types     | description                       |
| team_abbr               | character | Team abbreviation.                |
| team_name               | character | Team name.                        |
| team_common_name        | character | Team common name.                 |
| team_logo               | character | URL to the team logo image.       |
| conference_name         | character | Conference name.                  |
| division_abbrev         | character | Division abbreviation.            |
| division_name           | character | Division name.                    |
| place_name              | character | Place (city) name.                |
| conference_sequence     | integer   | Rank within the conference.       |
| division_sequence       | integer   | Rank within the division.         |
| league_sequence         | integer   | Rank within the league.           |
| wildcard_sequence       | integer   | Wildcard standing rank.           |
| games_played            | integer   | Games played.                     |
| wins                    | integer   | Wins.                             |
| losses                  | integer   | Losses.                           |
| ot_losses               | integer   | Overtime losses.                  |
| points                  | integer   | Standings points.                 |
| point_pctg              | numeric   | Points percentage.                |
| regulation_wins         | integer   | Regulation wins.                  |
| regulation_plus_ot_wins | integer   | Regulation plus overtime wins.    |
| goals_for               | integer   | Goals scored for.                 |
| goals_against           | integer   | Goals allowed against.            |
| goal_differential       | integer   | Goal differential.                |
| home_wins               | integer   | Home wins.                        |
| home_losses             | integer   | Home losses.                      |
| home_ot_losses          | integer   | Home overtime losses.             |
| road_wins               | integer   | Road wins.                        |
| road_losses             | integer   | Road losses.                      |
| road_ot_losses          | integer   | Road overtime losses.             |
| l10_wins                | integer   | Wins in last 10 games.            |
| l10_losses              | integer   | Losses in last 10 games.          |
| l10_ot_losses           | integer   | Overtime losses in last 10 games. |
| streak_code             | character | Current streak code (W/L/OT).     |
| streak_count            | integer   | Length of current streak.         |
| shootout_wins           | integer   | Shootout wins.                    |
| shootout_losses         | integer   | Shootout losses.                  |

## Examples

``` r
# \donttest{
  try(nhl_standings())
#> ── NHL Standings ────────────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-05-30 03:53:42 UTC
#> # A tibble: 32 × 36
#>    team_abbr team_name           team_common_name team_logo      conference_name
#>    <chr>     <chr>               <chr>            <chr>          <chr>          
#>  1 COL       Colorado Avalanche  Avalanche        https://asset… Western        
#>  2 CAR       Carolina Hurricanes Hurricanes       https://asset… Eastern        
#>  3 DAL       Dallas Stars        Stars            https://asset… Western        
#>  4 BUF       Buffalo Sabres      Sabres           https://asset… Eastern        
#>  5 TBL       Tampa Bay Lightning Lightning        https://asset… Eastern        
#>  6 MTL       Montréal Canadiens  Canadiens        https://asset… Eastern        
#>  7 MIN       Minnesota Wild      Wild             https://asset… Western        
#>  8 BOS       Boston Bruins       Bruins           https://asset… Eastern        
#>  9 OTT       Ottawa Senators     Senators         https://asset… Eastern        
#> 10 PIT       Pittsburgh Penguins Penguins         https://asset… Eastern        
#> # ℹ 22 more rows
#> # ℹ 31 more variables: division_abbrev <chr>, division_name <chr>,
#> #   place_name <chr>, conference_sequence <int>, division_sequence <int>,
#> #   league_sequence <int>, wildcard_sequence <int>, games_played <int>,
#> #   wins <int>, losses <int>, ot_losses <int>, points <int>, point_pctg <dbl>,
#> #   regulation_wins <int>, regulation_plus_ot_wins <int>, goals_for <int>,
#> #   goals_against <int>, goal_differential <int>, home_wins <int>, …
  try(nhl_standings(date = "2024-03-01"))
#> ── NHL Standings ────────────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-05-30 03:53:42 UTC
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
