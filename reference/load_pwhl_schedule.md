# **Load fastRhockey PWHL schedules**

Helper that loads multiple seasons of pre-scraped PWHL schedule data
from the
[sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
releases either into memory or writes it into a database.

## Usage

``` r
load_pwhl_schedule(
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
  season data into a database.

- dbConnection:

  A `DBIConnection` object, as returned by
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)

- tablename:

  The name of the schedule data table within the database

## Value

A data frame (`fastRhockey_data`) with the following columns:

|                 |           |                                            |
|-----------------|-----------|--------------------------------------------|
| col_name        | types     | description                                |
| game_id         | character | Unique game identifier.                    |
| season          | integer   | Season (concluding year, YYYY).            |
| game_date       | character | Game date.                                 |
| game_status     | character | Game status text.                          |
| home_team       | character | Home team name.                            |
| home_team_id    | character | Home team identifier.                      |
| away_team       | character | Away team name.                            |
| away_team_id    | character | Away team identifier.                      |
| home_score      | character | Home team final score.                     |
| away_score      | character | Away team final score.                     |
| winner          | character | Winning team.                              |
| venue           | character | Venue name.                                |
| venue_url       | character | Venue URL.                                 |
| game_type       | character | Game type the row belongs to.              |
| game_json       | logical   | Whether the game JSON is available.        |
| game_json_url   | glue      | URL to the game JSON feed.                 |
| PBP             | logical   | Whether play-by-play data is available.    |
| player_box      | logical   | Whether player box data is available.      |
| skater_box      | logical   | Whether skater box data is available.      |
| goalie_box      | logical   | Whether goalie box data is available.      |
| team_box        | logical   | Whether team box data is available.        |
| game_info       | logical   | Whether game info data is available.       |
| game_rosters    | logical   | Whether game rosters data is available.    |
| scoring_summary | logical   | Whether scoring summary data is available. |
| penalty_summary | logical   | Whether penalty summary data is available. |
| three_stars     | logical   | Whether three stars data is available.     |
| officials       | logical   | Whether officials data is available.       |
| shots_by_period | logical   | Whether shots-by-period data is available. |
| shootout        | logical   | Whether shootout data is available.        |

## Examples

``` r
# \donttest{
  try(load_pwhl_schedule(2024))
#> ─────────────────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> # A tibble: 85 × 29
#>    game_id season game_date   game_status home_team home_team_id away_team
#>    <chr>    <int> <chr>       <chr>       <chr>     <chr>        <chr>    
#>  1 84        2024 Wed, May 8  Final       Toronto   6            Minnesota
#>  2 98        2024 Wed, May 29 Final       Boston    1            Minnesota
#>  3 90        2024 Wed, May 15 Final OT2   Minnesota 2            Toronto  
#>  4 63        2024 Wed, May 1  Final       Toronto   6            Minnesota
#>  5 45        2024 Wed, Mar 6  Final       Toronto   6            Boston   
#>  6 46        2024 Wed, Mar 6  Final       New York  4            Montreal 
#>  7 52        2024 Wed, Mar 20 Final       Toronto   6            Boston   
#>  8 53        2024 Wed, Mar 20 Final       New York  4            Ottawa   
#>  9 50        2024 Wed, Mar 13 Final       Minnesota 2            Boston   
#> 10 4         2024 Wed, Jan 3  Final       Boston    1            Minnesota
#> # ℹ 75 more rows
#> # ℹ 22 more variables: away_team_id <chr>, home_score <chr>, away_score <chr>,
#> #   winner <chr>, venue <chr>, venue_url <chr>, game_type <chr>,
#> #   game_json <lgl>, game_json_url <glue>, PBP <lgl>, player_box <lgl>,
#> #   skater_box <lgl>, goalie_box <lgl>, team_box <lgl>, game_info <lgl>,
#> #   game_rosters <lgl>, scoring_summary <lgl>, penalty_summary <lgl>,
#> #   three_stars <lgl>, officials <lgl>, shots_by_period <lgl>, shootout <lgl>
# }
```
