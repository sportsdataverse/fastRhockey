# **PWHL Game Summary**

Retrieves a detailed game summary from the PWHL game center feed,
including scoring summary, penalty summary, shots by period, and three
stars.

## Usage

``` r
pwhl_game_summary(game_id)
```

## Arguments

- game_id:

  Game ID to retrieve the summary for.

## Value

A named list of data frames: `details`, `scoring`, `penalties`,
`shots_by_period`, `three_stars`.

**details**

|              |           |                                  |
|--------------|-----------|----------------------------------|
| col_name     | types     | description                      |
| game_id      | numeric   | Unique game identifier.          |
| date         | character | Game date.                       |
| status       | character | Game status.                     |
| venue        | character | Venue where the game was played. |
| attendance   | character | Reported attendance.             |
| home_team    | character | Home team name.                  |
| home_team_id | numeric   | Home team identifier.            |
| home_score   | numeric   | Home team final score.           |
| away_team    | character | Away team name.                  |
| away_team_id | numeric   | Away team identifier.            |
| away_score   | numeric   | Away team final score.           |

**scoring**

|           |           |                                      |
|-----------|-----------|--------------------------------------|
| col_name  | types     | description                          |
| period    | character | Period in which the goal was scored. |
| time      | character | Time of the goal.                    |
| team      | character | Team that scored.                    |
| team_id   | numeric   | Identifier of the scoring team.      |
| scorer    | character | Name of the goal scorer.             |
| scorer_id | numeric   | Identifier of the goal scorer.       |
| assist_1  | character | Name of the primary assister.        |
| assist_2  | character | Name of the secondary assister.      |
| goal_type | character | Type of goal.                        |

**penalties**

|            |           |                                       |
|------------|-----------|---------------------------------------|
| col_name   | types     | description                           |
| period     | character | Period in which the penalty occurred. |
| time       | character | Time of the penalty.                  |
| team       | character | Penalized team.                       |
| team_id    | numeric   | Identifier of the penalized team.     |
| player     | character | Penalized player name.                |
| player_id  | numeric   | Identifier of the penalized player.   |
| infraction | character | Penalty infraction description.       |
| minutes    | numeric   | Penalty minutes.                      |

**shots_by_period**

|            |           |                                |
|------------|-----------|--------------------------------|
| col_name   | types     | description                    |
| period     | character | Period number.                 |
| home_shots | numeric   | Home team shots in the period. |
| away_shots | numeric   | Away team shots in the period. |

**three_stars**

|            |           |                                    |
|------------|-----------|------------------------------------|
| col_name   | types     | description                        |
| star       | numeric   | Star ranking (1 to 3).             |
| player_id  | numeric   | Identifier of the selected player. |
| first_name | character | Player first name.                 |
| last_name  | character | Player last name.                  |
| team_id    | numeric   | Identifier of the player's team.   |

## Examples

``` r
# \donttest{
  try(pwhl_game_summary(game_id = 27))
#> $details
#>   game_id date status venue attendance home_team home_team_id home_score
#> 1      27 <NA>   <NA>  <NA>       <NA>      <NA>           NA          0
#>   away_team away_team_id away_score
#> 1      <NA>           NA          0
#> 
#> $scoring
#> data frame with 0 columns and 0 rows
#> 
#> $penalties
#> data frame with 0 columns and 0 rows
#> 
#> $shots_by_period
#> data frame with 0 columns and 0 rows
#> 
#> $three_stars
#> data frame with 0 columns and 0 rows
#> 
# }
```
