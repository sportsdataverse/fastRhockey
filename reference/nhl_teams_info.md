# **NHL Teams Info**

Returns NHL team information for a given team abbreviation. Uses the new
NHL API via
[nhl_teams](https://fastRhockey.sportsdataverse.org/reference/nhl_teams.md).

**Breaking change:** The old `team_id` (integer) parameter has been
replaced by `team_abbr` (3-letter string, e.g., "TBL") because the new
NHL API no longer exposes numeric team IDs as a primary identifier.

## Usage

``` r
nhl_teams_info(team_abbr)
```

## Arguments

- team_abbr:

  Three-letter team abbreviation (e.g., "TBL", "TOR", "SEA")

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
  try(nhl_teams_info(team_abbr = "TBL"))
#> ── NHL Teams Information from NHL.com ───────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-05-30 03:33:26 UTC
#> # A tibble: 1 × 20
#>   team_abbr team_name team_common_name team_logo conference_abbr conference_name
#>   <chr>     <chr>     <chr>            <chr>     <chr>           <chr>          
#> 1 TBL       Tampa Ba… Lightning        https://… Eastern         Eastern        
#> # ℹ 14 more variables: division_abbr <chr>, division_name <chr>,
#> #   place_name <chr>, games_played <int>, wins <int>, losses <int>,
#> #   ot_losses <int>, points <int>, point_pctg <dbl>, goals_for <int>,
#> #   goals_against <int>, goal_differential <int>, streak_code <chr>,
#> #   streak_count <int>
# }
```
