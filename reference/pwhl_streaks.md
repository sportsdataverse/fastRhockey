# **PWHL Player Streaks**

Retrieves player streak data for a PWHL season.

## Usage

``` r
pwhl_streaks(season = most_recent_pwhl_season(), game_type = "regular")
```

## Arguments

- season:

  Season (YYYY) to pull streaks from. Defaults to
  [`most_recent_pwhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_pwhl_season.md).

- game_type:

  Game type: "regular" (default), "preseason", or "playoffs".

## Value

A data frame of class `fastRhockey_data` with player streak data
including columns such as player_id, first_name, last_name, name,
length, goals, assists, points, games_played, rank, team_name,
team_code. Returns NULL if unavailable.

## Examples

``` r
# \donttest{
  try(pwhl_streaks(season = 2025))
#> ── PWHL Player Streaks ──────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:06:24 UTC
#> # A tibble: 20 × 25
#>    id    player_id rookie first_game_date last_game_date first_name   last_name 
#>    <chr> <chr>     <chr>  <chr>           <chr>          <chr>        <chr>     
#>  1 13037 205       1      Feb 23, 2025    Mar 16, 2025   Sarah        Fillier   
#>  2 13059 32        0      Feb 22, 2025    Mar  4, 2025   Laura        Stacey    
#>  3 12486 13        0      Mar 15, 2025    Mar 26, 2025   Hilary       Knight    
#>  4 12176 161       0      Feb 13, 2025    Feb 20, 2025   Tereza       Vanišová  
#>  5 11152 15        0      Feb 16, 2025    Feb 20, 2025   Alina        Müller    
#>  6 13128 157       0      Apr 26, 2025    present        Catherine    Dubois    
#>  7 12468 195       1      Dec  1, 2024    Dec  7, 2024   Dominique    Petrie    
#>  8 12098 53        0      Feb 22, 2025    Feb 26, 2025   Emily        Clark     
#>  9 12137 57        0      Feb 13, 2025    Feb 16, 2025   Gabbie       Hughes    
#> 10 12177 161       0      Mar 15, 2025    Mar 22, 2025   Tereza       Vanišová  
#> 11 13078 34        0      Dec  1, 2024    Dec  4, 2024   Alex         Carpenter 
#> 12 13124 31        0      Mar 23, 2025    Mar 26, 2025   Marie-Philip Poulin    
#> 13 12485 13        0      Feb 16, 2025    Feb 17, 2025   Hilary       Knight    
#> 14 12428 115       0      Dec  4, 2024    Dec  7, 2024   Michela      Cava      
#> 15 12125 63        0      Mar 26, 2025    Mar 30, 2025   Daryl        Watts     
#> 16 12434 78        0      Jan  2, 2025    Jan  5, 2025   Susanna      Tapani    
#> 17 12493 24        0      Apr 30, 2025    present        Lee          Stecklein 
#> 18 12087 210       1      Feb 16, 2025    Feb 19, 2025   Julia        Gosling   
#> 19 13122 31        0      Feb  2, 2025    Feb 15, 2025   Marie-Philip Poulin    
#> 20 13073 35        0      Mar 25, 2025    Apr  1, 2025   Jade         Downie-La…
#> # ℹ 18 more variables: name <chr>, length <chr>, goals <chr>, assists <chr>,
#> #   has_split <chr>, games_played <chr>, ongoing <chr>, points <chr>,
#> #   streak_start_team_id <chr>, streak_end_team_id <chr>, num_teams <chr>,
#> #   division_short_name <chr>, division_long_name <chr>, rank <chr>,
#> #   team_name <chr>, team_city <chr>, team_code <chr>, team_nickname <chr>
# }
```
