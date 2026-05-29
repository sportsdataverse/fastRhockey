# **PWHL Game Information**

PWHL Game Information

## Usage

``` r
pwhl_game_info(game_id)
```

## Arguments

- game_id:

  Game ID that you want game information for

## Value

A data frame (`fastRhockey_data`) with the following columns:

|                |           |                                          |
|----------------|-----------|------------------------------------------|
| col_name       | types     | description                              |
| game_id        | integer   | Unique game identifier.                  |
| game_season    | numeric   | Season (concluding year, YYYY).          |
| game_type      | character | Game type the row belongs to.            |
| game_date      | character | Game date.                               |
| home_team      | character | Home team name.                          |
| away_team      | character | Away team name.                          |
| home_team_id   | integer   | Home team identifier.                    |
| away_team_id   | integer   | Away team identifier.                    |
| home_score     | integer   | Home team final score.                   |
| away_score     | integer   | Away team final score.                   |
| game_duration  | character | Game duration.                           |
| game_venue     | character | Venue where the game was played.         |
| game_report    | character | URL to the game report.                  |
| game_boxscore  | character | URL to the text box score.               |
| game_season_id | character | Season identifier used by the PWHL feed. |

## Examples

``` r
# \donttest{
  try(pwhl_game_info(game_id = 27))
#>   game_id game_season game_type                 game_date   home_team
#> 1      27        2024   regular Sunday, February 04, 2024 PWHL Boston
#>       away_team home_team_id away_team_id home_score away_score game_duration
#> 1 PWHL Montreal            1            3          1          2          2:19
#>                game_venue
#> 1 Tsongas Center | Lowell
#>                                                                                                    game_report
#> 1 https://lscluster.hockeytech.com/game_reports/official-game-report.php?lang_id=1&client_code=pwhl&game_id=27
#>                                                                                              game_boxscore
#> 1 https://lscluster.hockeytech.com/game_reports/text-game-report.php?lang_id=1&client_code=pwhl&game_id=27
#>   game_season_id
#> 1              1
# }
```
