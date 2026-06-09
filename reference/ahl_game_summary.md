# **AHL Game Summary**

AHL game summary – named list of data frames (details, scoring,
penalties, shots_by_period, three_stars).

## Usage

``` r
ahl_game_summary(game_id)
```

## Arguments

- game_id:

  Numeric AHL game identifier.

## Value

A named list of data frames.

## See also

Other AHL Functions:
[`ahl`](https://fastRhockey.sportsdataverse.org/reference/ahl.md),
[`ahl_game_corsi()`](https://fastRhockey.sportsdataverse.org/reference/ahl_game_corsi.md),
[`ahl_game_shifts()`](https://fastRhockey.sportsdataverse.org/reference/ahl_game_shifts.md),
[`ahl_leaders()`](https://fastRhockey.sportsdataverse.org/reference/ahl_leaders.md),
[`ahl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/ahl_pbp.md),
[`ahl_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/ahl_player_stats.md),
[`ahl_player_toi()`](https://fastRhockey.sportsdataverse.org/reference/ahl_player_toi.md),
[`ahl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/ahl_schedule.md),
[`ahl_season_id()`](https://fastRhockey.sportsdataverse.org/reference/ahl_season_id.md),
[`ahl_standings()`](https://fastRhockey.sportsdataverse.org/reference/ahl_standings.md),
[`ahl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/ahl_team_roster.md),
[`ahl_teams()`](https://fastRhockey.sportsdataverse.org/reference/ahl_teams.md),
[`most_recent_ahl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_ahl_season.md)

## Examples

``` r
 try(ahl_game_summary(game_id = 1000093924)) 
#> $game
#> # A tibble: 1 × 6
#>      game_id date  status venue home_team away_team
#>        <dbl> <chr> <chr>  <chr> <chr>     <chr>    
#> 1 1000093924 NA    NA     NA    NA        NA       
#> 
#> $goals
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
```
