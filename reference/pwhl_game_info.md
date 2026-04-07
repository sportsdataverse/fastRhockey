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

A data frame with game id / date, game metadata etc from the PWHL

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
