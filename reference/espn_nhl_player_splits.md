# **Get ESPN NHL Player Splits**

**Get ESPN NHL Player Splits**

## Usage

``` r
espn_nhl_player_splits(athlete_id, season = most_recent_nhl_season(), ...)
```

## Arguments

- athlete_id:

  ESPN athlete identifier (character or numeric, e.g. `"5149125"`).
  Obtain via
  [`espn_nhl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_team_roster.md).

- season:

  Season end-year (e.g. `2025`). Defaults to
  [`most_recent_nhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_nhl_season.md).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per situational split:

|  |  |  |
|----|----|----|
| col_name | types | description |
| athlete_id | character | ESPN athlete identifier (echoed from arg). |
| season | integer | Season end-year (echoed from arg). |
| split_category | character | Split category name (e.g. split, location). |
| split_category_display | character | Split category display name. |
| split_display_name | character | Individual split label (e.g. All Splits). |
| split_abbreviation | character | Split abbreviation. |
| games | character | Games played. |
| goals | character | Goals. |
| assists | character | Assists. |
| points | character | Points. |
| plus_minus | character | Plus/minus. |
| penalty_minutes | character | Penalty minutes. |
| shots_total | character | Shots on goal. |
| faceoff_percent | character | Faceoff percentage. |
| power_play_goals | character | Power-play goals. |
| power_play_assists | character | Power-play assists. |
| short_handed_goals | character | Short-handed goals. |
| short_handed_assists | character | Short-handed assists. |
| game_winning_goals | character | Game-winning goals. |
| time_on_ice_per_game | character | Average time on ice per game. |
| production | character | Production metric. |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_player_splits(athlete_id = "5149125", season = 2025))
#> ── NHL Player Splits data from ESPN.com ─────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 20:16:46 UTC
#> # A tibble: 62 × 21
#>    athlete_id season split_category split_category_display split_display_name   
#>    <chr>       <int> <chr>          <chr>                  <chr>                
#>  1 5149125      2025 split          split                  All Splits           
#>  2 5149125      2025 split          split                  Home                 
#>  3 5149125      2025 split          split                  Away                 
#>  4 5149125      2025 split          split                  Day                  
#>  5 5149125      2025 split          split                  Night                
#>  6 5149125      2025 byOpponent     Opponent               Boston Bruins        
#>  7 5149125      2025 byOpponent     Opponent               vs Buffalo Sabres    
#>  8 5149125      2025 byOpponent     Opponent               vs Detroit Red Wings 
#>  9 5149125      2025 byOpponent     Opponent               vs Florida Panthers  
#> 10 5149125      2025 byOpponent     Opponent               vs Montreal Canadiens
#> # ℹ 52 more rows
#> # ℹ 16 more variables: split_abbreviation <chr>, games <chr>, goals <chr>,
#> #   assists <chr>, points <chr>, plus_minus <chr>, penalty_minutes <chr>,
#> #   shots_total <chr>, faceoff_percent <chr>, power_play_goals <chr>,
#> #   power_play_assists <chr>, short_handed_goals <chr>,
#> #   short_handed_assists <chr>, game_winning_goals <chr>,
#> #   time_on_ice_per_game <chr>, production <chr>
# }
```
