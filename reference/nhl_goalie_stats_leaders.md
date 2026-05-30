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

A data frame (`fastRhockey_data`) with the following columns:

|                    |           |                                     |
|--------------------|-----------|-------------------------------------|
| col_name           | types     | description                         |
| id                 | integer   | Unique player identifier.           |
| sweater_number     | integer   | Jersey number.                      |
| headshot           | character | Player headshot image URL.          |
| team_abbrev        | character | Team abbreviation.                  |
| team_logo          | character | Team logo URL.                      |
| position           | character | Player position.                    |
| value              | numeric   | Statistical value for the category. |
| first_name_default | character | Player first name (default locale). |
| last_name_default  | character | Player last name (default locale).  |
| team_name_default  | character | Team name (default locale).         |
| first_name_cs      | character | Player first name (Czech locale).   |
| first_name_sk      | character | Player first name (Slovak locale).  |
| last_name_cs       | character | Player last name (Czech locale).    |
| last_name_sk       | character | Player last name (Slovak locale).   |
| category           | character | Stat leader category.               |
| last_name_fi       | character | Player last name (Finnish locale).  |
| team_name_fr       | character | Team name (French locale).          |

## Examples

``` r
# \donttest{
  try(nhl_goalie_stats_leaders())
#> ── NHL Goalie Stats Leaders ─────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-05-30 03:09:37 UTC
#> # A tibble: 18 × 17
#>         id sweater_number headshot         team_abbrev team_logo position  value
#>      <int>          <int> <chr>            <chr>       <chr>     <chr>     <dbl>
#>  1 8479394             79 https://assets.… VGK         https://… G        12    
#>  2 8475883             31 https://assets.… CAR         https://… G        12    
#>  3 8482487             75 https://assets.… MTL         https://… G         9    
#>  4 8475809             41 https://assets.… COL         https://… G         7    
#>  5 8480843              1 https://assets.… ANA         https://… G         6    
#>  6 8475883             31 https://assets.… CAR         https://… G         3    
#>  7 8478435             80 https://assets.… PHI         https://… G         2    
#>  8 8476883             88 https://assets.… TBL         https://… G         1    
#>  9 8481668             37 https://assets.… PIT         https://… G         0.939
#> 10 8476999             35 https://assets.… OTT         https://… G         0.932
#> 11 8475883             31 https://assets.… CAR         https://… G         0.931
#> 12 8479394             79 https://assets.… VGK         https://… G         0.924
#> 13 8478435             80 https://assets.… PHI         https://… G         0.922
#> 14 8475883             31 https://assets.… CAR         https://… G         1.41 
#> 15 8481668             37 https://assets.… PIT         https://… G         1.52 
#> 16 8476999             35 https://assets.… OTT         https://… G         2.03 
#> 17 8478024             33 https://assets.… ANA         https://… G         2.12 
#> 18 8478435             80 https://assets.… PHI         https://… G         2.18 
#> # ℹ 10 more variables: first_name_default <chr>, last_name_default <chr>,
#> #   team_name_default <chr>, first_name_cs <chr>, first_name_sk <chr>,
#> #   last_name_cs <chr>, last_name_sk <chr>, category <chr>, last_name_fi <chr>,
#> #   team_name_fr <chr>
# }
```
