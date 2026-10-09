# **NHL Teams**

Returns current NHL team information. Uses the NHL API standings
endpoint to get up-to-date team info.

## Usage

``` r
nhl_teams(season = NULL)
```

## Arguments

- season:

  Integer four-digit year the season starts in (e.g., 2024 for 2024-25;
  [`most_recent_nhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_nhl_season.md)
  returns the year it ends). If NULL, returns current teams.

## Value

A data frame (`fastRhockey_data`) with the following columns:

|                   |           |                               |
|-------------------|-----------|-------------------------------|
| col_name          | types     | description                   |
| team_abbr         | character | Team abbreviation.            |
| team_name         | character | Team name.                    |
| team_common_name  | character | Team common name.             |
| team_logo         | character | URL to the team logo image.   |
| conference_abbr   | character | Conference abbreviation.      |
| conference_name   | character | Conference name.              |
| division_abbr     | character | Division abbreviation.        |
| division_name     | character | Division name.                |
| place_name        | character | Team place/city name.         |
| games_played      | integer   | Games played.                 |
| wins              | integer   | Wins.                         |
| losses            | integer   | Losses.                       |
| ot_losses         | integer   | Overtime losses.              |
| points            | integer   | Total points.                 |
| point_pctg        | numeric   | Points percentage.            |
| goals_for         | integer   | Goals for.                    |
| goals_against     | integer   | Goals against.                |
| goal_differential | integer   | Goal differential.            |
| streak_code       | character | Current streak code (W/L/OT). |
| streak_count      | integer   | Length of the current streak. |

## Examples

``` r
# \donttest{
  try(nhl_teams())
#> ── NHL Teams ────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-10-09 03:21:54 UTC
#> # A tibble: 32 × 20
#>    team_abbr team_name            team_common_name team_logo     conference_abbr
#>    <chr>     <chr>                <chr>            <chr>         <chr>          
#>  1 NYR       New York Rangers     Rangers          https://asse… Eastern        
#>  2 EDM       Edmonton Oilers      Oilers           https://asse… Western        
#>  3 WPG       Winnipeg Jets        Jets             https://asse… Western        
#>  4 CAR       Carolina Hurricanes  Hurricanes       https://asse… Eastern        
#>  5 VGK       Vegas Golden Knights Golden Knights   https://asse… Western        
#>  6 OTT       Ottawa Senators      Senators         https://asse… Eastern        
#>  7 TBL       Tampa Bay Lightning  Lightning        https://asse… Eastern        
#>  8 FLA       Florida Panthers     Panthers         https://asse… Eastern        
#>  9 SJS       San Jose Sharks      Sharks           https://asse… Western        
#> 10 UTA       Utah Mammoth         Mammoth          https://asse… Western        
#> # ℹ 22 more rows
#> # ℹ 15 more variables: conference_name <chr>, division_abbr <chr>,
#> #   division_name <chr>, place_name <chr>, games_played <int>, wins <int>,
#> #   losses <int>, ot_losses <int>, points <int>, point_pctg <dbl>,
#> #   goals_for <int>, goals_against <int>, goal_differential <int>,
#> #   streak_code <chr>, streak_count <int>
# }
```
