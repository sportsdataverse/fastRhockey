# **Load fastRhockey PWHL game info**

Helper that loads multiple seasons of per-game PWHL metadata (one row
per game) from the
[sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
releases either into memory or writes it into a database.

Source release tag: `pwhl_game_info`. File naming convention:
`game_info_{end_year}.rds`.

## Usage

``` r
load_pwhl_game_info(
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

  The name of the game info data table within the database

## Value

A data frame (`fastRhockey_data`) with the following columns:

|                 |           |                                             |
|-----------------|-----------|---------------------------------------------|
| col_name        | types     | description                                 |
| game_id         | integer   | Unique game identifier.                     |
| game_number     | character | League game number.                         |
| game_date       | character | Human-readable game date.                   |
| game_date_iso   | character | ISO-8601 game start datetime.               |
| start_time      | character | Start time (local).                         |
| end_time        | character | End time (local).                           |
| game_duration   | character | Game length (H:MM).                         |
| game_venue      | character | Venue name.                                 |
| attendance      | integer   | Reported attendance.                        |
| game_status     | character | Game status text.                           |
| game_season_id  | integer   | HockeyTech season identifier.               |
| started         | integer   | Flag for whether the game has started.      |
| final           | integer   | Flag for whether the game is final.         |
| home_team_id    | integer   | Home team identifier.                       |
| home_team       | character | Home team name.                             |
| home_team_abbr  | character | Home team abbreviation.                     |
| home_score      | integer   | Home team final score.                      |
| away_team_id    | integer   | Away team identifier.                       |
| away_team       | character | Away team name.                             |
| away_team_abbr  | character | Away team abbreviation.                     |
| away_score      | integer   | Away team final score.                      |
| has_shootout    | integer   | Flag for whether the game went to shootout. |
| game_report_url | character | URL to the game report.                     |
| boxscore_url    | character | URL to the boxscore.                        |

## Examples

``` r
# \donttest{
  try(load_pwhl_game_info(2024))
#> ─────────────────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> # A tibble: 85 × 24
#>    game_id game_number game_date game_date_iso start_time end_time game_duration
#>      <int> <chr>       <chr>     <chr>         <chr>      <chr>    <chr>        
#>  1       2 1           Monday, … 2024-01-01T1… 12:48 pm … 3:15 pm… 2:27         
#>  2       3 2           Tuesday,… 2024-01-02T1… 7:12 pm E… 9:35 pm… 2:23         
#>  3       4 3           Wednesda… 2024-01-03T1… 7:14 pm E… 9:35 pm… 2:21         
#>  4       5 4           Friday, … 2024-01-05T1… 7:15 pm E… 9:40 pm… 2:25         
#>  5       6 5           Saturday… 2024-01-06T1… 2:42 pm C… 5:03 pm… 2:21         
#>  6       8 7           Wednesda… 2024-01-10T1… 7:08 pm C… 9:28 pm… 2:20         
#>  7       9 8           Wednesda… 2024-01-10T1… 7:11 pm E… 9:37 pm… 2:26         
#>  8      10 9           Saturday… 2024-01-13T1… 1:09 pm E… 3:25 pm… 2:16         
#>  9      11 10          Saturday… 2024-01-13T1… 3:48 pm E… 6:18 pm… 2:30         
#> 10      12 11          Sunday, … 2024-01-14T1… 3:08 pm C… 5:32 pm… 2:24         
#> # ℹ 75 more rows
#> # ℹ 17 more variables: game_venue <chr>, attendance <int>, game_status <chr>,
#> #   game_season_id <int>, started <int>, final <int>, home_team_id <int>,
#> #   home_team <chr>, home_team_abbr <chr>, home_score <int>,
#> #   away_team_id <int>, away_team <chr>, away_team_abbr <chr>,
#> #   away_score <int>, has_shootout <int>, game_report_url <chr>,
#> #   boxscore_url <chr>
# }
```
