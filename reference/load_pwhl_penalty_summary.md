# **Load fastRhockey PWHL penalty summaries**

Helper that loads multiple seasons of per-penalty PWHL penalty summaries
(one row per penalty) from the
[sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
releases either into memory or writes it into a database.

Source release tag: `pwhl_penalty_summary`. File naming convention:
`penalty_summary_{end_year}.rds`.

## Usage

``` r
load_pwhl_penalty_summary(
  seasons = most_recent_pwhl_season(),
  ...,
  dbConnection = NULL,
  tablename = NULL
)
```

## Arguments

- seasons:

  A vector of 4-digit years associated with given PWHL seasons. (Min:
  2024)

- ...:

  Additional arguments (currently unused; kept for API symmetry).

- dbConnection:

  A `DBIConnection` object, as returned by
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)

- tablename:

  The name of the penalty summary data table within the database

## Value

A data frame (`fastRhockey_data`) with the following columns:

|                   |           |                                                |
|-------------------|-----------|------------------------------------------------|
| col_name          | types     | description                                    |
| game_id           | integer   | Unique game identifier.                        |
| period_id         | integer   | Period identifier.                             |
| period            | character | Period long name.                              |
| time              | character | Game clock at infraction (MM:SS).              |
| team_id           | integer   | Penalized team identifier.                     |
| team              | character | Penalized team name.                           |
| team_abbr         | character | Penalized team abbreviation.                   |
| game_penalty_id   | integer   | Penalty identifier within the game.            |
| minutes           | integer   | Penalty length in minutes.                     |
| description       | character | Infraction description.                        |
| rule_number       | character | Rulebook rule number.                          |
| is_power_play     | integer   | Power-play flag.                               |
| is_bench          | integer   | Bench-minor flag.                              |
| taken_by_id       | integer   | Identifier of the player who took the penalty. |
| taken_by_first    | character | Offender first name.                           |
| taken_by_last     | character | Offender last name.                            |
| taken_by_position | character | Offender position.                             |
| served_by_id      | integer   | Identifier of the player serving the penalty.  |
| served_by_first   | character | First name of the player serving.              |
| served_by_last    | character | Last name of the player serving.               |

## Examples

``` r
# \donttest{
  try(load_pwhl_penalty_summary(2024))
#> ─────────────────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> # A tibble: 518 × 20
#>    game_id period_id period time  team_id team         team_abbr game_penalty_id
#>      <int>     <int> <chr>  <chr>   <int> <chr>        <chr>               <int>
#>  1       2         1 1st    13:23       6 PWHL Toronto TOR                    73
#>  2       2         1 1st    15:21       4 PWHL New Yo… NY                     74
#>  3       2         1 1st    19:59       4 PWHL New Yo… NY                     75
#>  4       2         3 3rd    08:49       6 PWHL Toronto TOR                    76
#>  5       2         3 3rd    11:54       4 PWHL New Yo… NY                     77
#>  6       2         3 3rd    12:39       4 PWHL New Yo… NY                     78
#>  7       3         1 1st    03:22       3 PWHL Montre… MTL                    79
#>  8       3         1 1st    08:37       3 PWHL Montre… MTL                    81
#>  9       3         1 1st    10:20       3 PWHL Montre… MTL                    80
#> 10       3         2 2nd    07:28       3 PWHL Montre… MTL                    84
#> # ℹ 508 more rows
#> # ℹ 12 more variables: minutes <int>, description <chr>, rule_number <chr>,
#> #   is_power_play <int>, is_bench <int>, taken_by_id <int>,
#> #   taken_by_first <chr>, taken_by_last <chr>, taken_by_position <chr>,
#> #   served_by_id <int>, served_by_first <chr>, served_by_last <chr>
# }
```
