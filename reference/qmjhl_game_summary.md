# **QMJHL Game Summary**

QMJHL game summary – named list of data frames (details, scoring,
penalties, shots_by_period, three_stars).

## Usage

``` r
qmjhl_game_summary(game_id)
```

## Arguments

- game_id:

  Numeric QMJHL game identifier.

## Value

A named list of data frames.

## See also

Other QMJHL Functions:
[`most_recent_qmjhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_qmjhl_season.md),
[`qmjhl`](https://fastRhockey.sportsdataverse.org/reference/qmjhl.md),
[`qmjhl_game_corsi()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_game_corsi.md),
[`qmjhl_game_shifts()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_game_shifts.md),
[`qmjhl_leaders()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_leaders.md),
[`qmjhl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_pbp.md),
[`qmjhl_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_player_stats.md),
[`qmjhl_player_toi()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_player_toi.md),
[`qmjhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_schedule.md),
[`qmjhl_season_id()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_season_id.md),
[`qmjhl_standings()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_standings.md),
[`qmjhl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_team_roster.md),
[`qmjhl_teams()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_teams.md)

## Examples

``` r
 try(qmjhl_game_summary(game_id = 27225)) 
#> $game
#> # A tibble: 1 × 10
#>   game_id date          status venue home_team home_team_id home_score away_team
#>     <dbl> <chr>         <chr>  <chr> <chr>     <chr>             <int> <chr>    
#> 1   27225 Saturday, Ap… Final  Cent… Drummond… 14                    7 Sherbroo…
#> # ℹ 2 more variables: away_team_id <chr>, away_score <int>
#> 
#> $goals
#> # A tibble: 8 × 36
#>   event x_location y_location time  team_id home  period_id goal_type
#>   <chr> <chr>      <chr>      <chr> <chr>   <chr> <chr>     <chr>    
#> 1 goal  73         157        9:35  14      1     1         ""       
#> 2 goal  85         155        10:59 14      1     1         ""       
#> 3 goal  106        169        11:04 14      1     1         ""       
#> 4 goal  125        64         12:45 14      1     1         "PP"     
#> 5 goal  75         174        18:17 14      1     1         ""       
#> 6 goal  623        147        0:23  14      1     2         "PP"     
#> 7 goal  48         187        1:36  60      0     2         ""       
#> 8 goal  649        170        4:26  14      1     2         ""       
#> # ℹ 28 more variables: location_set <chr>, power_play <chr>, empty_net <chr>,
#> #   penalty_shot <chr>, short_handed <chr>, insurance_goal <chr>,
#> #   game_winning <chr>, game_tieing <chr>, scorer_goal_num <chr>, s <int>,
#> #   goal_scorer_player_id <chr>, goal_scorer_jersey_number <chr>,
#> #   goal_scorer_team_id <chr>, goal_scorer_team_code <chr>,
#> #   goal_scorer_first_name <chr>, goal_scorer_last_name <chr>,
#> #   assist1_player_player_id <chr>, assist1_player_jersey_number <chr>, …
#> 
#> $penalties
#> # A tibble: 19 × 28
#>    event   time_off_formatted team_id home  period_id period offence pp    bench
#>    <chr>   <chr>              <chr>   <chr> <chr>     <chr>  <chr>   <chr> <chr>
#>  1 penalty 1:43               60      0     1         1st    394     1     0    
#>  2 penalty 10:41              60      0     1         1st    403     0     0    
#>  3 penalty 10:41              14      1     1         1st    403     0     0    
#>  4 penalty 11:15              60      0     1         1st    403     0     0    
#>  5 penalty 11:15              60      0     1         1st    398     1     0    
#>  6 penalty 11:15              60      0     1         1st    305     0     0    
#>  7 penalty 11:15              14      1     1         1st    305     0     0    
#>  8 penalty 11:15              14      1     1         1st    403     0     0    
#>  9 penalty 16:09              14      1     1         1st    388     1     0    
#> 10 penalty 18:34              60      0     1         1st    361     1     0    
#> 11 penalty 18:34              60      0     1         1st    403     0     0    
#> 12 penalty 18:34              60      0     1         1st    403     0     0    
#> 13 penalty 18:34              14      1     1         1st    403     0     0    
#> 14 penalty 18:34              14      1     1         1st    403     0     0    
#> 15 penalty 8:30               60      0     2         2nd    344     1     0    
#> 16 penalty 9:36               60      0     2         2nd    364     1     0    
#> 17 penalty 9:36               60      0     2         2nd    364     1     0    
#> 18 penalty 14:20              14      1     2         2nd    388     1     0    
#> 19 penalty 16:47              14      1     3         3rd    347     1     0    
#> # ℹ 19 more variables: penalty_shot <chr>, minutes <int>,
#> #   minutes_formatted <chr>, penalty_class_id <chr>, penalty_class <chr>,
#> #   lang_penalty_description <chr>, s <int>,
#> #   player_penalized_info_player_id <chr>,
#> #   player_penalized_info_jersey_number <chr>,
#> #   player_penalized_info_team_id <chr>, player_penalized_info_team_code <chr>,
#> #   player_penalized_info_first_name <chr>, …
#> 
#> $shots_by_period
#> # A tibble: 6 × 3
#>   side    period shots
#>   <chr>   <chr>  <int>
#> 1 visitor 1          7
#> 2 visitor 2          8
#> 3 visitor 3          7
#> 4 home    1         26
#> 5 home    2         14
#> 6 home    3         10
#> 
#> $three_stars
#> # A tibble: 3 × 5
#>   player_id first_name last_name jersey_number  home
#>   <chr>     <chr>      <chr>     <chr>         <int>
#> 1 17001     Dawson     Mercer    19                1
#> 2 16938     Gregor     MacLeod   23                1
#> 3 16769     Pavel      Koltygin  27                1
#> 
```
