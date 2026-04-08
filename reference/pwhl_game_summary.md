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

A named list with the following elements:

- `details` - Data frame with one row of game metadata (teams, date,
  venue, score).

- `scoring` - Data frame of goals with period, time, team, scorer, and
  assists.

- `penalties` - Data frame of penalties with period, time, team, player,
  and infraction.

- `shots_by_period` - Data frame of shots per period for each team.

- `three_stars` - Data frame with the game's three stars.

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
