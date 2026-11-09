# **Get ESPN MCH / WCH Standings**

**Get ESPN MCH / WCH Standings**

## Usage

``` r
espn_mch_standings(
  season = most_recent_nhl_season(),
  group = NULL,
  standings_type = NULL,
  ...
)

espn_wch_standings(
  season = most_recent_nhl_season(),
  group = NULL,
  standings_type = NULL,
  ...
)
```

## Arguments

- season:

  Season end-year (e.g. `2025`). Defaults to
  [`most_recent_nhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_nhl_season.md).

- group:

  Optional conference/group id filter.

- standings_type:

  Optional standings variant sent as `type` (e.g. `"by-division"`).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per team:

|                    |           |                                     |
|--------------------|-----------|-------------------------------------|
| col_name           | type      | description                         |
| group_name         | character | Group name (conference / division). |
| group_abbreviation | character | Group abbreviation.                 |
| team_id            | character | ESPN team identifier.               |
| team_name          | character | Team name.                          |
| team_abbreviation  | character | Team abbreviation.                  |
| team_display_name  | character | Team display name.                  |
| team_location      | character | Team location (city).               |
| team_logo          | character | Team logo URL.                      |
| ot_losses          | numeric   | Overtime losses.                    |
| ot_wins            | numeric   | Overtime wins.                      |
| avg_points_against | numeric   | Average goals against per game.     |
| avg_points_for     | numeric   | Average goals for per game.         |
| differential       | numeric   | Goal differential.                  |
| games_behind       | numeric   | Games behind the group leader.      |
| games_played       | numeric   | Games played.                       |
| losses             | numeric   | Regulation losses.                  |
| playoff_seed       | numeric   | Current playoff seed.               |
| point_differential | numeric   | Point (goal) differential.          |
| points             | numeric   | Points.                             |
| points_against     | numeric   | Goals/points allowed.               |
| points_for         | numeric   | Goals/points scored.                |
| streak             | numeric   | Current streak value.               |
| tiegames           | numeric   | Tie games (sent in the mch body).   |
| win_percent        | numeric   | Win percentage.                     |
| wins               | numeric   | Wins.                               |
| overall            | numeric   | Overall record stat.                |
| season             | integer   | Season end-year (echoed from arg).  |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_standings()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_standings.md);
columns verified against a captured MCH body One numeric column per ESPN
stat in the body, so the set follows the feed (the MCH body sends
`tiegames`, the WCH body `ties`).

`espn_wch_standings()` is the same wrapper for women's college hockey
(league slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_mch_standings(season = 2025))
#> ── MENS-COLLEGE-HOCKEY Standings data from ESPN.com ─────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 11:49:33 UTC
#> # A tibble: 1 × 27
#>   group_name         group_abbreviation team_id team_name team_abbreviation
#>   <chr>              <chr>              <chr>   <chr>     <chr>            
#> 1 Big Ten Conference Big Ten            194     Buckeyes  OSU              
#> # ℹ 22 more variables: team_display_name <chr>, team_location <chr>,
#> #   team_logo <chr>, ot_losses <dbl>, ot_wins <dbl>, avg_points_against <dbl>,
#> #   avg_points_for <dbl>, differential <dbl>, games_behind <dbl>,
#> #   games_played <dbl>, losses <dbl>, playoff_seed <dbl>,
#> #   point_differential <dbl>, points <dbl>, points_against <dbl>,
#> #   points_for <dbl>, streak <dbl>, tiegames <dbl>, win_percent <dbl>,
#> #   wins <dbl>, overall <dbl>, season <int>
# }
# \donttest{
  try(espn_wch_standings(season = 2025))
#> ── WOMENS-COLLEGE-HOCKEY Standings data from ESPN.com ───── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 11:49:34 UTC
#> # A tibble: 11 × 27
#>    group_name          group_abbreviation team_id team_name    team_abbreviation
#>    <chr>               <chr>              <chr>   <chr>        <chr>            
#>  1 NCAA Women's Hockey CWHOC              172     Big Red      COR              
#>  2 NCAA Women's Hockey CWHOC              104     Terriers     BU               
#>  3 NCAA Women's Hockey CWHOC              134     Bulldogs     UMD              
#>  4 NCAA Women's Hockey CWHOC              194     Buckeyes     OSU              
#>  5 NCAA Women's Hockey CWHOC              213     Nittany Lio… PSU              
#>  6 NCAA Women's Hockey CWHOC              2779    Saints       USL              
#>  7 NCAA Women's Hockey CWHOC              275     Badgers      WIS              
#>  8 NCAA Women's Hockey CWHOC              2137    Golden Knig… CLAR             
#>  9 NCAA Women's Hockey CWHOC              2142    Raiders      COLG             
#> 10 NCAA Women's Hockey CWHOC              2529    Pioneers     SHU              
#> 11 NCAA Women's Hockey CWHOC              135     Golden Goph… MINN             
#> # ℹ 22 more variables: team_display_name <chr>, team_location <chr>,
#> #   team_logo <chr>, ot_losses <dbl>, ot_wins <dbl>, avg_points_against <dbl>,
#> #   avg_points_for <dbl>, differential <dbl>, games_behind <dbl>,
#> #   games_played <dbl>, losses <dbl>, playoff_seed <dbl>,
#> #   point_differential <dbl>, points <dbl>, points_against <dbl>,
#> #   points_for <dbl>, streak <dbl>, ties <dbl>, win_percent <dbl>, wins <dbl>,
#> #   overall <dbl>, season <int>
# }
```
