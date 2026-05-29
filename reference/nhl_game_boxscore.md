# **NHL Game Boxscore**

Retrieve boxscore data for a specific NHL game from the NHL web API
(`api-web.nhle.com`).

## Usage

``` r
nhl_game_boxscore(game_id)
```

## Arguments

- game_id:

  *(integer)* NHL game ID, e.g. `2024020001`.

## Value

A named list of data frames: `game_info`, `team_box`, `skater_stats`,
`goalie_stats`.

**game_info**

|                  |           |                                               |
|------------------|-----------|-----------------------------------------------|
| col_name         | types     | description                                   |
| game_id          | integer   | Unique game identifier.                       |
| season           | integer   | Season (concluding year, YYYY).               |
| game_type        | integer   | Game type code (2 = regular, 3 = playoffs).   |
| game_date        | character | Game date.                                    |
| venue            | character | Venue name.                                   |
| game_state       | character | Game state (e.g., FINAL, LIVE).               |
| away_team_id     | integer   | Away team identifier.                         |
| away_team_abbrev | character | Away team abbreviation.                       |
| away_team_name   | character | Away team name.                               |
| away_score       | integer   | Away team final score.                        |
| away_sog         | integer   | Away team shots on goal.                      |
| home_team_id     | integer   | Home team identifier.                         |
| home_team_abbrev | character | Home team abbreviation.                       |
| home_team_name   | character | Home team name.                               |
| home_score       | integer   | Home team final score.                        |
| home_sog         | integer   | Home team shots on goal.                      |
| last_period_type | character | Type of the last period played (REG, OT, SO). |

**team_box**

|                  |           |                         |
|------------------|-----------|-------------------------|
| col_name         | types     | description             |
| home_away        | character | Home or away indicator. |
| team_id          | integer   | Unique team identifier. |
| team_abbrev      | character | Team abbreviation.      |
| team_name        | character | Team name.              |
| goals            | integer   | Goals scored.           |
| shots_on_goal    | integer   | Shots on goal.          |
| pim              | integer   | Penalty minutes.        |
| hits             | integer   | Hits.                   |
| blocked_shots    | integer   | Blocked shots.          |
| giveaways        | integer   | Giveaways.              |
| takeaways        | integer   | Takeaways.              |
| power_play_goals | integer   | Power-play goals.       |
| faceoff_win_pctg | numeric   | Faceoff win percentage. |
| saves            | integer   | Saves made.             |
| save_pctg        | numeric   | Save percentage.        |
| goals_against    | integer   | Goals against.          |

**skater_stats**

|                      |           |                                 |
|----------------------|-----------|---------------------------------|
| col_name             | types     | description                     |
| home_away            | character | Home or away indicator.         |
| team_id              | integer   | Unique team identifier.         |
| team_abbrev          | character | Team abbreviation.              |
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
| power_play_goals     | integer   | Power-play goals.               |
| shots_on_goal        | integer   | Shots on goal.                  |
| faceoff_winning_pctg | numeric   | Faceoff win percentage.         |
| toi                  | character | Time on ice.                    |
| blocked_shots        | integer   | Blocked shots.                  |
| shifts               | integer   | Number of shifts.               |
| giveaways            | integer   | Giveaways.                      |
| takeaways            | integer   | Takeaways.                      |

**goalie_stats**

|  |  |  |
|----|----|----|
| col_name | types | description |
| home_away | character | Home or away indicator. |
| team_id | integer | Unique team identifier. |
| team_abbrev | character | Team abbreviation. |
| player_id | integer | Unique player identifier. |
| player_name | character | Player name. |
| sweater_number | integer | Jersey number. |
| even_strength_shots_against | character | Even-strength shots against (saves/total). |
| power_play_shots_against | character | Power-play shots against (saves/total). |
| shorthanded_shots_against | character | Shorthanded shots against (saves/total). |
| save_shots_against | character | Total shots against (saves/total). |
| save_pctg | numeric | Save percentage. |
| even_strength_goals_against | integer | Even-strength goals against. |
| power_play_goals_against | integer | Power-play goals against. |
| shorthanded_goals_against | integer | Shorthanded goals against. |
| pim | integer | Penalty minutes. |
| goals_against | integer | Goals against. |
| toi | character | Time on ice. |
| starter | logical | Whether the goalie started the game. |
| decision | character | Goalie decision (W, L, O). |
| shots_against | integer | Shots faced. |
| saves | integer | Saves made. |

## Details

Uses the endpoint
`https://api-web.nhle.com/v1/gamecenter/{game_id}/boxscore`.

## Examples

``` r
if (FALSE) { # \dontrun{
  box <- nhl_game_boxscore(2024020001)
  box$game_info
  box$skater_stats
} # }
```
