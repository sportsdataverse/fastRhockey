# **PWHL Player Streaks**

Retrieves player streak data for a PWHL season.

## Usage

``` r
pwhl_streaks(season = most_recent_pwhl_season(), game_type = "both")
```

## Arguments

- season:

  Season (YYYY) to pull streaks from. Defaults to
  [`most_recent_pwhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_pwhl_season.md).

- game_type:

  Game type: `"both"` (default), `"regular"`, `"playoffs"`, or
  `"preseason"`. When `"both"`, regular-season and playoff streaks are
  combined and labeled by a `game_type` column. Game types with no
  streak data for the season are omitted rather than erroring.

## Value

A data frame (`fastRhockey_data`) with the following columns:

|                      |           |                                    |
|----------------------|-----------|------------------------------------|
| col_name             | types     | description                        |
| id                   | character | Unique streak identifier.          |
| player_id            | character | Unique player identifier.          |
| rookie               | character | Whether the player is a rookie.    |
| first_game_date      | character | Date of the streak's first game.   |
| last_game_date       | character | Date of the streak's last game.    |
| first_name           | character | Player first name.                 |
| last_name            | character | Player last name.                  |
| name                 | character | Player full name.                  |
| length               | character | Length of the streak in games.     |
| goals                | character | Goals scored during the streak.    |
| assists              | character | Assists during the streak.         |
| has_split            | character | Whether the streak spans teams.    |
| games_played         | character | Games played during the streak.    |
| ongoing              | character | Whether the streak is ongoing.     |
| points               | character | Total points during the streak.    |
| streak_start_team_id | character | Team identifier at streak start.   |
| streak_end_team_id   | character | Team identifier at streak end.     |
| num_teams            | character | Number of teams during the streak. |
| division_short_name  | character | Short division name.               |
| division_long_name   | character | Full division name.                |
| rank                 | character | Rank of the streak.                |
| team_name            | character | Team name.                         |
| team_city            | character | Team city.                         |
| team_code            | character | Team abbreviation.                 |
| team_nickname        | character | Team nickname.                     |
| game_type            | character | Game type the row belongs to.      |

## Examples

``` r
# \donttest{
  try(pwhl_streaks(season = 2025))
#> ── PWHL Player Streaks ──────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-06-09 20:30:28 UTC
#> # A tibble: 26 × 26
#>    id    player_id rookie first_game_date last_game_date first_name last_name
#>    <chr> <chr>     <chr>  <chr>           <chr>          <chr>      <chr>    
#>  1 23158 205       1      Feb 23, 2025    Mar 16, 2025   Sarah      Fillier  
#>  2 23407 32        0      Feb 22, 2025    Mar  4, 2025   Laura      Stacey   
#>  3 23389 13        0      Mar 15, 2025    Mar 26, 2025   Hilary     Knight   
#>  4 23288 161       0      Feb 13, 2025    Feb 20, 2025   Tereza     Vanišová 
#>  5 23422 15        0      Feb 16, 2025    Feb 20, 2025   Alina      Müller   
#>  6 23414 157       0      Apr 26, 2025    present        Catherine  Dubois   
#>  7 23226 195       1      Dec  1, 2024    Dec  7, 2024   Dominique  Petrie   
#>  8 23327 53        0      Feb 22, 2025    Feb 26, 2025   Emily      Clark    
#>  9 23317 57        0      Feb 13, 2025    Feb 16, 2025   Gabbie     Hughes   
#> 10 23289 161       0      Mar 15, 2025    Mar 22, 2025   Tereza     Vanišová 
#> # ℹ 16 more rows
#> # ℹ 19 more variables: name <chr>, length <chr>, goals <chr>, assists <chr>,
#> #   has_split <chr>, games_played <chr>, ongoing <chr>, points <chr>,
#> #   streak_start_team_id <chr>, streak_end_team_id <chr>, num_teams <chr>,
#> #   division_short_name <chr>, division_long_name <chr>, rank <chr>,
#> #   team_name <chr>, team_city <chr>, team_code <chr>, team_nickname <chr>,
#> #   game_type <chr>
# }
```
