# **Load fastRhockey NHL skater box scores**

Helper that loads multiple seasons of pre-scraped NHL skater box scores
from the
[sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
releases.

## Usage

``` r
load_nhl_skater_box(
  seasons = most_recent_nhl_season(),
  ...,
  dbConnection = NULL,
  tablename = NULL
)
```

## Arguments

- seasons:

  A vector of 4-digit years (the *end year* of the NHL season; e.g.,
  2026 for the 2025-26 season). Min: 2011.

- ...:

  Additional arguments passed to an underlying function.

- dbConnection:

  A `DBIConnection` object, as returned by
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)

- tablename:

  The name of the data table within the database

## Value

A data frame (`fastRhockey_data`) with the following columns:

|                      |           |                                 |
|----------------------|-----------|---------------------------------|
| col_name             | types     | description                     |
| home_away            | character | Home or away indicator.         |
| team_id              | integer   | Unique team identifier.         |
| team_abbrev          | character | Team abbreviation/code.         |
| player_id            | integer   | Unique player identifier.       |
| player_name          | character | Player name.                    |
| sweater_number       | integer   | Jersey number.                  |
| position             | character | Player position.                |
| goals                | integer   | Goals scored.                   |
| assists              | integer   | Assists.                        |
| points               | integer   | Total points (goals + assists). |
| plus_minus           | integer   | Plus/minus rating.              |
| pim                  | integer   | Penalty minutes.                |
| hits                 | integer   | Hits.                           |
| power_play_goals     | integer   | Power play goals.               |
| shots_on_goal        | integer   | Shots on goal.                  |
| faceoff_winning_pctg | numeric   | Faceoff win percentage.         |
| toi                  | character | Time on ice.                    |
| blocked_shots        | integer   | Blocked shots.                  |
| shifts               | integer   | Number of shifts.               |
| giveaways            | integer   | Giveaways.                      |
| takeaways            | integer   | Takeaways.                      |

## Examples

``` r
# \donttest{
  try(load_nhl_skater_box(2022))
#> ── NHL skater boxscores ─────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-09-09 07:37:00 UTC
#> # A tibble: 50,415 × 24
#>    home_away team_id team_abbrev player_id player_name  sweater_number position
#>    <chr>       <int> <chr>           <int> <chr>                 <int> <chr>   
#>  1 away            5 PIT           8478542 E. Rodrigues              9 C       
#>  2 away            5 PIT           8482055 D. O'Connor              10 L       
#>  3 away            5 PIT           8470619 B. Boyle                 11 C       
#>  4 away            5 PIT           8475722 J. Zucker                16 L       
#>  5 away            5 PIT           8475810 B. Rust                  17 R       
#>  6 away            5 PIT           8478043 S. Lafferty              18 C       
#>  7 away            5 PIT           8476934 B. McGinn                23 L       
#>  8 away            5 PIT           8477953 K. Kapanen               42 R       
#>  9 away            5 PIT           8478046 D. Heinen                43 L       
#> 10 away            5 PIT           8478866 D. Simon                 49 C       
#> # ℹ 50,405 more rows
#> # ℹ 17 more variables: goals <int>, assists <int>, points <int>,
#> #   plus_minus <int>, pim <int>, hits <int>, power_play_goals <int>,
#> #   shots_on_goal <int>, faceoff_winning_pctg <dbl>, toi <chr>,
#> #   blocked_shots <int>, shifts <int>, giveaways <int>, takeaways <int>,
#> #   game_id <int>, season <int>, game_date <chr>
# }
```
