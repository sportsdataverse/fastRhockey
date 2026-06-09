# **OHL Game Summary**

OHL game summary – named list of data frames (details, scoring,
penalties, shots_by_period, three_stars).

## Usage

``` r
ohl_game_summary(game_id)
```

## Arguments

- game_id:

  Numeric OHL game identifier.

## Value

A named list of data frames.

## See also

Other OHL Functions:
[`most_recent_ohl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_ohl_season.md),
[`ohl`](https://fastRhockey.sportsdataverse.org/reference/ohl.md),
[`ohl_game_corsi()`](https://fastRhockey.sportsdataverse.org/reference/ohl_game_corsi.md),
[`ohl_game_shifts()`](https://fastRhockey.sportsdataverse.org/reference/ohl_game_shifts.md),
[`ohl_leaders()`](https://fastRhockey.sportsdataverse.org/reference/ohl_leaders.md),
[`ohl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/ohl_pbp.md),
[`ohl_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/ohl_player_stats.md),
[`ohl_player_toi()`](https://fastRhockey.sportsdataverse.org/reference/ohl_player_toi.md),
[`ohl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/ohl_schedule.md),
[`ohl_season_id()`](https://fastRhockey.sportsdataverse.org/reference/ohl_season_id.md),
[`ohl_standings()`](https://fastRhockey.sportsdataverse.org/reference/ohl_standings.md),
[`ohl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/ohl_team_roster.md),
[`ohl_teams()`](https://fastRhockey.sportsdataverse.org/reference/ohl_teams.md)

## Examples

``` r
 try(ohl_game_summary(game_id = 27225)) 
#> $game
#> # A tibble: 1 × 10
#>   game_id date          status venue home_team home_team_id home_score away_team
#>     <dbl> <chr>         <chr>  <chr> <chr>     <chr>             <int> <chr>    
#> 1   27225 Monday, Apri… Final  Harr… Owen Sou… 11                    1 Saginaw …
#> # ℹ 2 more variables: away_team_id <chr>, away_score <int>
#> 
#> $goals
#> # A tibble: 3 × 36
#>   event x_location y_location time  team_id home  period_id goal_type
#>   <chr> <chr>      <chr>      <chr> <chr>   <chr> <chr>     <chr>    
#> 1 goal  83         174        19:46 34      0     1         ""       
#> 2 goal  79         139        17:18 11      1     2         "PP"     
#> 3 goal  628        143        19:49 34      0     2         ""       
#> # ℹ 28 more variables: location_set <chr>, power_play <chr>, empty_net <chr>,
#> #   penalty_shot <chr>, short_handed <chr>, insurance_goal <chr>,
#> #   game_winning <chr>, game_tieing <chr>, scorer_goal_num <chr>, s <int>,
#> #   goal_scorer_player_id <chr>, goal_scorer_jersey_number <chr>,
#> #   goal_scorer_team_id <chr>, goal_scorer_team_code <chr>,
#> #   goal_scorer_first_name <chr>, goal_scorer_last_name <chr>,
#> #   assist1_player_player_id <chr>, assist1_player_jersey_number <chr>, …
#> 
#> $penalties
#> # A tibble: 8 × 28
#>   event   time_off_formatted team_id home  period_id period offence pp    bench
#>   <chr>   <chr>              <chr>   <chr> <chr>     <chr>  <chr>   <chr> <chr>
#> 1 penalty 3:50               34      0     1         1st    7       1     0    
#> 2 penalty 8:45               11      1     1         1st    17      1     0    
#> 3 penalty 13:32              34      0     1         1st    30      1     0    
#> 4 penalty 15:53              11      1     1         1st    14      1     0    
#> 5 penalty 16:02              34      0     2         2nd    15      1     0    
#> 6 penalty 17:46              11      1     2         2nd    34      1     0    
#> 7 penalty 6:43               11      1     3         3rd    5       1     0    
#> 8 penalty 14:58              11      1     3         3rd    33      1     1    
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
#> 1 visitor 1         12
#> 2 visitor 2         21
#> 3 visitor 3          9
#> 4 home    1          7
#> 5 home    2          8
#> 6 home    3         10
#> 
#> $three_stars
#> # A tibble: 3 × 5
#>   player_id first_name last_name jersey_number  home
#>   <chr>     <chr>      <chr>     <chr>         <int>
#> 1 8819      Carter     George    32                1
#> 2 8770      Joey       Willis    14                0
#> 3 8369      Andrew     Oke       29                0
#> 
```
