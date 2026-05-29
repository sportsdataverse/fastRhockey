# **Load fastRhockey PWHL play-by-play**

Helper that loads multiple seasons of pre-scraped PWHL play-by-play data
from the
[sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
releases either into memory or writes it into a database.

## Usage

``` r
load_pwhl_pbp(
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

  Additional arguments passed to an underlying function that writes the
  season data into a database (used by
  [`update_pwhl_db()`](https://fastRhockey.sportsdataverse.org/reference/update_pwhl_db.md)).

- dbConnection:

  A `DBIConnection` object, as returned by
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)

- tablename:

  The name of the play-by-play data table within the database

## Value

A data frame (`fastRhockey_data`) with the following columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| game_id | integer | Unique game identifier. |
| event | character | Play-by-play event description. |
| team_id | integer | Team identifier for the event. |
| period_of_game | character | Period in which the event occurred. |
| time_of_period | character | Game clock time within the period. |
| player_id | integer | Primary player identifier. |
| player_name_first | character | Primary player first name. |
| player_name_last | character | Primary player last name. |
| player_position | character | Primary player position. |
| player_two_id | integer | Second player identifier. |
| player_two_name_first | character | Second player first name. |
| player_two_name_last | character | Second player last name. |
| player_two_position | character | Second player position. |
| x_coord | numeric | Event x-coordinate on the ice. |
| y_coord | numeric | Event y-coordinate on the ice. |
| home_win | integer | Flag for whether the home team won. |
| player_team_id | integer | Team identifier of the primary player. |
| event_type | character | Categorized event type. |
| shot_quality | character | Shot quality descriptor. |
| goal | logical | Flag for whether the event was a goal. |
| goalie_id | integer | Goalie identifier on the play. |
| goalie_first | character | Goalie first name. |
| goalie_last | character | Goalie last name. |
| player_three_id | integer | Third player identifier. |
| player_three_name_first | character | Third player first name. |
| player_three_name_last | character | Third player last name. |
| player_three_position | character | Third player position. |
| empty_net | character | Empty-net flag. |
| game_winner | character | Game-winning-goal flag. |
| penalty_shot | character | Penalty-shot flag. |
| insurance | character | Insurance-goal flag. |
| power_play | integer | Power-play flag. |
| short_handed | character | Short-handed flag. |
| plus_player_one_id | integer | On-ice plus player one identifier. |
| plus_player_one_first | character | On-ice plus player one first name. |
| plus_player_one_last | character | On-ice plus player one last name. |
| plus_player_one_position | character | On-ice plus player one position. |
| plus_player_two_id | integer | On-ice plus player two identifier. |
| plus_player_two_first | character | On-ice plus player two first name. |
| plus_player_two_last | character | On-ice plus player two last name. |
| plus_player_two_position | character | On-ice plus player two position. |
| plus_player_three_id | integer | On-ice plus player three identifier. |
| plus_player_three_first | character | On-ice plus player three first name. |
| plus_player_three_last | character | On-ice plus player three last name. |
| plus_player_three_position | character | On-ice plus player three position. |
| plus_player_four_id | integer | On-ice plus player four identifier. |
| plus_player_four_first | character | On-ice plus player four first name. |
| plus_player_four_last | character | On-ice plus player four last name. |
| plus_player_four_position | character | On-ice plus player four position. |
| plus_player_five_id | integer | On-ice plus player five identifier. |
| plus_player_five_first | character | On-ice plus player five first name. |
| plus_player_five_last | character | On-ice plus player five last name. |
| plus_player_five_position | character | On-ice plus player five position. |
| minus_player_one_id | integer | On-ice minus player one identifier. |
| minus_player_one_first | character | On-ice minus player one first name. |
| minus_player_one_last | character | On-ice minus player one last name. |
| minus_player_one_position | character | On-ice minus player one position. |
| minus_player_two_id | integer | On-ice minus player two identifier. |
| minus_player_two_first | character | On-ice minus player two first name. |
| minus_player_two_last | character | On-ice minus player two last name. |
| minus_player_two_position | character | On-ice minus player two position. |
| minus_player_three_id | integer | On-ice minus player three identifier. |
| minus_player_three_first | character | On-ice minus player three first name. |
| minus_player_three_last | character | On-ice minus player three last name. |
| minus_player_three_position | character | On-ice minus player three position. |
| minus_player_four_id | integer | On-ice minus player four identifier. |
| minus_player_four_first | character | On-ice minus player four first name. |
| minus_player_four_last | character | On-ice minus player four last name. |
| minus_player_four_position | character | On-ice minus player four position. |
| minus_player_five_id | integer | On-ice minus player five identifier. |
| minus_player_five_first | character | On-ice minus player five first name. |
| minus_player_five_last | character | On-ice minus player five last name. |
| minus_player_five_position | character | On-ice minus player five position. |
| penalty_length | character | Penalty length in minutes. |
| game_date | character | Game date. |
| game_season | integer | Season (concluding year, YYYY). |
| game_season_id | character | HockeyTech season identifier. |
| home_team_id | integer | Home team identifier. |
| home_team | character | Home team name. |
| away_team_id | integer | Away team identifier. |
| away_team | character | Away team name. |
| x_coord_original | integer | Original raw x-coordinate. |
| y_coord_original | integer | Original raw y-coordinate. |
| x_coord_neutral | integer | Neutral-orientation x-coordinate. |
| y_coord_neutral | integer | Neutral-orientation y-coordinate. |
| x_coord_fixed | numeric | Fixed-orientation x-coordinate. |
| y_coord_fixed | numeric | Fixed-orientation y-coordinate. |
| x_coord_right | numeric | Right-orientation x-coordinate. |
| y_coord_right | numeric | Right-orientation y-coordinate. |
| x_coord_vertical | numeric | Vertical-orientation x-coordinate. |
| y_coord_vertical | numeric | Vertical-orientation y-coordinate. |
| minute_start | integer | Minute the event started. |
| second_start | integer | Second the event started. |
| clock | character | Game clock string. |
| sec_from_start | integer | Seconds elapsed from the start of the game. |

## Examples

``` r
# \donttest{
  try(load_pwhl_pbp(2024))
#> ─────────────────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> # A tibble: 10,456 × 95
#>    game_id event   team_id period_of_game time_of_period player_id
#>      <int> <chr>     <int> <chr>          <chr>              <int>
#>  1       2 faceoff      NA 1              0:00                  76
#>  2       2 shot          4 1              1:29                  44
#>  3       2 shot          4 1              3:30                  36
#>  4       2 shot          4 1              3:36                  46
#>  5       2 faceoff      NA 1              4:30                  73
#>  6       2 shot          4 1              4:57                  86
#>  7       2 faceoff      NA 1              5:18                  73
#>  8       2 shot          4 1              6:42                  38
#>  9       2 shot          6 1              7:05                 128
#> 10       2 shot          6 1              9:12                 126
#> # ℹ 10,446 more rows
#> # ℹ 89 more variables: player_name_first <chr>, player_name_last <chr>,
#> #   player_position <chr>, player_two_id <int>, player_two_name_first <chr>,
#> #   player_two_name_last <chr>, player_two_position <chr>, x_coord <dbl>,
#> #   y_coord <dbl>, home_win <int>, player_team_id <int>, event_type <chr>,
#> #   shot_quality <chr>, goal <lgl>, goalie_id <int>, goalie_first <chr>,
#> #   goalie_last <chr>, player_three_id <int>, player_three_name_first <chr>, …
# }
```
