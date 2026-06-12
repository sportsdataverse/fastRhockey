# **PWHL Player Game Log**

Retrieves game-by-game statistics for a PWHL player in a given season.

## Usage

``` r
pwhl_player_game_log(
  player_id,
  season = most_recent_pwhl_season(),
  game_type = "both"
)
```

## Arguments

- player_id:

  Numeric player ID

- season:

  Season (YYYY) to pull the game log from, the concluding year in
  XXXX-YY format. Defaults to
  [`most_recent_pwhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_pwhl_season.md).

- game_type:

  Game type: `"both"` (default), `"regular"`, `"playoffs"`, or
  `"preseason"`. When `"both"`, the player's regular-season and playoff
  game logs are combined and labeled by a `game_type` column. Game types
  the player has no games in (e.g. playoffs for a non-qualifying team)
  are omitted rather than erroring.

## Value

A data frame (`fastRhockey_data`) with the following columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| g_month | character | Month the game was played. |
| id | numeric | Unique game identifier. |
| home_team | character | Home team identifier. |
| visiting_team | character | Visiting team identifier. |
| date_played | character | Date the game was played. |
| home | numeric | Whether the player's team was home. |
| goalie | numeric | Whether the player was the goalie. |
| home_team_code | character | Home team abbreviation. |
| home_team_name | character | Home team name. |
| home_division | character | Home team division. |
| visiting_team_code | character | Visiting team abbreviation. |
| visiting_team_name | character | Visiting team name. |
| visiting_division | character | Visiting team division. |
| goals_against | numeric | Goals against (goalie). |
| seconds_played | character | Seconds played in the game. |
| win | numeric | Whether the game was a win (goalie). |
| tie | character | Whether the game was a tie (goalie). |
| loss | numeric | Whether the game was a loss (goalie). |
| total_losses | character | Total losses to date (goalie). |
| shutout | character | Whether the game was a shutout (goalie). |
| ot_loss | character | Whether the game was an overtime loss. |
| shootout_loss | character | Whether the game was a shootout loss. |
| saves | numeric | Saves made (goalie). |
| shots_against | numeric | Shots faced (goalie). |
| shootout_saves | character | Shootout saves made. |
| shootout_goals_against | character | Shootout goals against. |
| shootout_shots | character | Shootout shots faced. |
| goals | numeric | Goals scored. |
| assists | numeric | Assists. |
| pim | character | Penalty minutes. |
| points | numeric | Total points (goals + assists). |
| gaa | character | Goals against average (goalie). |
| svpct | character | Save percentage (goalie). |
| shootout_shots_percentage | character | Shootout save percentage. |
| penalty_minutes | numeric | Penalty minutes. |
| minutes | character | Minutes played. |
| player_team | character | The player's team. |
| player_id | numeric | Unique player identifier. |
| game_type | character | Game type the row belongs to ("regular"/"playoffs"). |

## Examples

``` r
# \donttest{
  try(pwhl_player_game_log(player_id = 28, season = 2025))
#> ── PWHL Player Game Log ─────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-06-12 14:21:11 UTC
#> # A tibble: 25 × 39
#>    g_month    id home_team visiting_team date_played  home goalie home_team_code
#>    <chr>   <dbl> <chr>     <chr>         <chr>       <dbl>  <dbl> <chr>         
#>  1 Novemb…   106 3         5             2024-11-30      1      1 MTL           
#>  2 Decemb…   110 3         4             2024-12-04      1      1 MTL           
#>  3 Decemb…   119 2         3             2024-12-28      0      1 MIN           
#>  4 Decemb…   121 3         1             2024-12-30      1      1 MTL           
#>  5 January   173 1         3             2025-01-05      0      1 BOS           
#>  6 January   174 6         3             2025-01-08      0      1 TOR           
#>  7 January   131 3         2             2025-01-17      1      1 MTL           
#>  8 January   176 3         5             2025-01-19      1      1 MTL           
#>  9 January   137 3         5             2025-01-29      1      1 MTL           
#> 10 Februa…   141 4         3             2025-02-02      0      1 NY            
#> # ℹ 15 more rows
#> # ℹ 31 more variables: home_team_name <chr>, home_division <chr>,
#> #   visiting_team_code <chr>, visiting_team_name <chr>,
#> #   visiting_division <chr>, goals_against <dbl>, seconds_played <chr>,
#> #   win <dbl>, tie <chr>, loss <dbl>, total_losses <chr>, shutout <chr>,
#> #   ot_loss <chr>, shootout_loss <chr>, saves <dbl>, shots_against <dbl>,
#> #   shootout_saves <chr>, shootout_goals_against <chr>, shootout_shots <chr>, …
# }
```
