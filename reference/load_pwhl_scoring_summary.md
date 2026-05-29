# **Load fastRhockey PWHL scoring summaries**

Helper that loads multiple seasons of per-goal PWHL scoring summaries
(one row per goal) from the
[sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
releases either into memory or writes it into a database.

Source release tag: `pwhl_scoring_summary`. File naming convention:
`scoring_summary_{end_year}.rds`.

## Usage

``` r
load_pwhl_scoring_summary(
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

  The name of the scoring summary data table within the database

## Value

A data frame (`fastRhockey_data`) with the following columns:

|                    |           |                                      |
|--------------------|-----------|--------------------------------------|
| col_name           | types     | description                          |
| game_id            | integer   | Unique game identifier.              |
| period_id          | integer   | Period identifier.                   |
| period             | character | Period long name (e.g. 1st, 1st OT). |
| time               | character | Game clock at goal (MM:SS).          |
| team_id            | integer   | Scoring team identifier.             |
| team               | character | Scoring team name.                   |
| team_abbr          | character | Scoring team abbreviation.           |
| game_goal_id       | integer   | Goal identifier within the game.     |
| scorer_goal_number | integer   | Scorer's season goal number.         |
| scorer_id          | integer   | Goal scorer identifier.              |
| scorer_first       | character | Scorer first name.                   |
| scorer_last        | character | Scorer last name.                    |
| scorer_position    | character | Scorer position.                     |
| assist_1_id        | integer   | Primary assist player identifier.    |
| assist_1_first     | character | Primary assist first name.           |
| assist_1_last      | character | Primary assist last name.            |
| assist_2_id        | integer   | Secondary assist player identifier.  |
| assist_2_first     | character | Secondary assist first name.         |
| assist_2_last      | character | Secondary assist last name.          |
| is_power_play      | integer   | Power-play flag.                     |
| is_short_handed    | integer   | Short-handed flag.                   |
| is_empty_net       | integer   | Empty-net flag.                      |
| is_penalty_shot    | integer   | Penalty-shot flag.                   |
| is_insurance       | integer   | Insurance-goal flag.                 |
| is_game_winning    | integer   | Game-winning-goal flag.              |
| x_location         | logical   | Goal x-coordinate on the ice.        |
| y_location         | logical   | Goal y-coordinate on the ice.        |

## Examples

``` r
# \donttest{
  try(load_pwhl_scoring_summary(2024))
#> ─────────────────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> # A tibble: 385 × 27
#>    game_id period_id period time  team_id team           team_abbr game_goal_id
#>      <int>     <int> <chr>  <chr>   <int> <chr>          <chr>            <int>
#>  1       2         1 1st    10:43       4 PWHL New York  NY                  64
#>  2       2         3 3rd    2:53        4 PWHL New York  NY                  65
#>  3       2         3 3rd    4:57        4 PWHL New York  NY                  66
#>  4       2         3 3rd    7:42        4 PWHL New York  NY                  67
#>  5       3         2 2nd    16:24       5 PWHL Ottawa    OTT                 68
#>  6       3         2 2nd    17:45       3 PWHL Montreal  MTL                 69
#>  7       3         3 3rd    5:18        5 PWHL Ottawa    OTT                 70
#>  8       3         3 3rd    14:23       3 PWHL Montreal  MTL                 71
#>  9       3         4 1st OT 1:04        3 PWHL Montreal  MTL                 72
#> 10       4         1 1st    3:58        2 PWHL Minnesota MIN                 73
#> # ℹ 375 more rows
#> # ℹ 19 more variables: scorer_goal_number <int>, scorer_id <int>,
#> #   scorer_first <chr>, scorer_last <chr>, scorer_position <chr>,
#> #   assist_1_id <int>, assist_1_first <chr>, assist_1_last <chr>,
#> #   assist_2_id <int>, assist_2_first <chr>, assist_2_last <chr>,
#> #   is_power_play <int>, is_short_handed <int>, is_empty_net <int>,
#> #   is_penalty_shot <int>, is_insurance <int>, is_game_winning <int>, …
# }
```
