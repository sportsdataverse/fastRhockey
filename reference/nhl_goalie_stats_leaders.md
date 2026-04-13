# **NHL Goalie Stats Leaders**

Returns league-wide goalie statistical leaders for a given season and
game type. Supports multiple stat categories.

## Usage

``` r
nhl_goalie_stats_leaders(
  season = NULL,
  game_type = 2,
  categories = NULL,
  limit = NULL
)
```

## Arguments

- season:

  Integer 4-digit year (e.g., 2024 for the 2024-25 season). If NULL,
  returns current season leaders.

- game_type:

  Integer game type: 2 = regular season (default), 3 = playoffs

- categories:

  Character vector of stat categories (e.g., "wins", "gaa", "savePctg",
  "shutouts"). If NULL, returns all available categories.

- limit:

  Integer maximum number of leaders per category. If NULL, uses API
  default.

## Value

Returns a data frame with goalie leaders.

## Examples

``` r
# \donttest{
  try(nhl_goalie_stats_leaders())
#> ── NHL Goalie Stats Leaders ─────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:05:19 UTC
#> # A tibble: 20 × 17
#>         id sweater_number headshot         team_abbrev team_logo position  value
#>      <int>          <int> <chr>            <chr>       <chr>     <chr>     <dbl>
#>  1 8476883             88 https://assets.… TBL         https://… G        38    
#>  2 8478872             70 https://assets.… UTA         https://… G        37    
#>  3 8479979             29 https://assets.… DAL         https://… G        34    
#>  4 8480313             48 https://assets.… WSH         https://… G        31    
#>  5 8480843              1 https://assets.… ANA         https://… G        30    
#>  6 8478009             30 https://assets.… NYI         https://… G         7    
#>  7 8480981             30 https://assets.… STL         https://… G         6    
#>  8 8482661             30 https://assets.… MIN         https://… G         4    
#>  9 8479406             32 https://assets.… MIN         https://… G         4    
#> 10 8475683             72 https://assets.… FLA         https://… G         4    
#> 11 8475809             41 https://assets.… COL         https://… G         0.918
#> 12 8482661             30 https://assets.… MIN         https://… G         0.914
#> 13 8476883             88 https://assets.… TBL         https://… G         0.913
#> 14 8480313             48 https://assets.… WSH         https://… G         0.912
#> 15 8478048             31 https://assets.… NYR         https://… G         0.912
#> 16 8475809             41 https://assets.… COL         https://… G         2.10 
#> 17 8476883             88 https://assets.… TBL         https://… G         2.30 
#> 18 8479193              1 https://assets.… DAL         https://… G         2.34 
#> 19 8478435             80 https://assets.… PHI         https://… G         2.44 
#> 20 8480313             48 https://assets.… WSH         https://… G         2.44 
#> # ℹ 10 more variables: first_name_default <chr>, first_name_cs <chr>,
#> #   first_name_sk <chr>, last_name_default <chr>, last_name_cs <chr>,
#> #   last_name_fi <chr>, last_name_sk <chr>, team_name_default <chr>,
#> #   category <chr>, first_name_fi <chr>
# }
```
