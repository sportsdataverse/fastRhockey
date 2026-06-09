# **WHL Game Summary**

WHL game summary – named list of data frames (details, scoring,
penalties, shots_by_period, three_stars).

## Usage

``` r
whl_game_summary(game_id)
```

## Arguments

- game_id:

  Numeric WHL game identifier.

## Value

A named list of data frames.

## See also

Other WHL Functions:
[`most_recent_whl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_whl_season.md),
[`whl`](https://fastRhockey.sportsdataverse.org/reference/whl.md),
[`whl_game_corsi()`](https://fastRhockey.sportsdataverse.org/reference/whl_game_corsi.md),
[`whl_game_shifts()`](https://fastRhockey.sportsdataverse.org/reference/whl_game_shifts.md),
[`whl_leaders()`](https://fastRhockey.sportsdataverse.org/reference/whl_leaders.md),
[`whl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/whl_pbp.md),
[`whl_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/whl_player_stats.md),
[`whl_player_toi()`](https://fastRhockey.sportsdataverse.org/reference/whl_player_toi.md),
[`whl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/whl_schedule.md),
[`whl_season_id()`](https://fastRhockey.sportsdataverse.org/reference/whl_season_id.md),
[`whl_standings()`](https://fastRhockey.sportsdataverse.org/reference/whl_standings.md),
[`whl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/whl_team_roster.md),
[`whl_teams()`](https://fastRhockey.sportsdataverse.org/reference/whl_teams.md)

## Examples

``` r
 try(whl_game_summary(game_id = 27225)) 
#> $game
#> # A tibble: 1 × 10
#>   game_id date          status venue home_team home_team_id home_score away_team
#>     <dbl> <chr>         <chr>  <chr> <chr>     <chr>             <int> <chr>    
#> 1   27225 Sunday, Dece… Final  Keys… Brandon … 201                   2 Kelowna …
#> # ℹ 2 more variables: away_team_id <chr>, away_score <int>
#> 
#> $goals
#> # A tibble: 7 × 38
#>   event x_location y_location time  team_id home  period_id goal_type
#>   <chr> <chr>      <chr>      <chr> <chr>   <chr> <chr>     <chr>    
#> 1 goal  ""         ""         1:21  204     0     1         ""       
#> 2 goal  ""         ""         19:17 201     1     1         "PP"     
#> 3 goal  ""         ""         8:52  201     1     2         ""       
#> 4 goal  ""         ""         16:51 204     0     2         "PP"     
#> 5 goal  ""         ""         3:55  204     0     3         ""       
#> 6 goal  ""         ""         8:16  204     0     3         ""       
#> 7 goal  ""         ""         10:04 204     0     3         ""       
#> # ℹ 30 more variables: location_set <chr>, power_play <chr>, empty_net <chr>,
#> #   penalty_shot <chr>, short_handed <chr>, insurance_goal <chr>,
#> #   game_winning <chr>, game_tieing <chr>, scorer_goal_num <chr>, s <int>,
#> #   goal_scorer_player_id <chr>, goal_scorer_jersey_number <chr>,
#> #   goal_scorer_team_id <chr>, goal_scorer_team_code <chr>,
#> #   goal_scorer_first_name <chr>, goal_scorer_last_name <chr>,
#> #   assist1_player_player_id <chr>, assist1_player_jersey_number <chr>, …
#> 
#> $penalties
#> # A tibble: 18 × 28
#>    event   time_off_formatted team_id home  period_id period offence pp    bench
#>    <chr>   <chr>              <chr>   <chr> <chr>     <chr>  <chr>   <chr> <chr>
#>  1 penalty 6:35               201     1     1         1st    22      1     0    
#>  2 penalty 14:05              204     0     1         1st    30      1     0    
#>  3 penalty 14:53              201     1     1         1st    30      1     0    
#>  4 penalty 15:25              201     1     1         1st    30      1     0    
#>  5 penalty 17:35              204     0     1         1st    22      1     0    
#>  6 penalty 18:59              204     0     1         1st    30      0     0    
#>  7 penalty 18:59              201     1     1         1st    30      0     0    
#>  8 penalty 0:29               204     0     2         2nd    8       1     0    
#>  9 penalty 5:52               204     0     2         2nd    54      0     0    
#> 10 penalty 5:52               201     1     2         2nd    54      0     0    
#> 11 penalty 6:44               204     0     2         2nd    54      0     0    
#> 12 penalty 6:44               204     0     2         2nd    6       1     0    
#> 13 penalty 6:44               201     1     2         2nd    54      0     0    
#> 14 penalty 12:54              201     1     2         2nd    16      1     0    
#> 15 penalty 16:26              201     1     2         2nd    79      1     0    
#> 16 penalty 17:31              204     0     2         2nd    18      1     0    
#> 17 penalty 19:19              204     0     2         2nd    8       1     0    
#> 18 penalty 14:03              201     1     3         3rd    8       1     0    
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
#> 2 visitor 2          7
#> 3 visitor 3          9
#> 4 home    1          9
#> 5 home    2          8
#> 6 home    3          5
#> 
#> $three_stars
#> # A tibble: 3 × 5
#>   player_id first_name last_name jersey_number  home
#>   <chr>     <chr>      <chr>     <chr>         <int>
#> 1 23858     Justin     Keller    19                0
#> 2 21562     Tyler      Dyck      12                1
#> 3 22733     Tyler      Spurgeon  15                0
#> 
```
