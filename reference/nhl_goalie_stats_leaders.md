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
#> ── NHL Goalie Stats Leaders ────────────────────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 06:16:19 UTC
#> # A tibble: 20 × 18
#>         id sweater_number headshot          team_abbrev team_logo position value
#>      <int>          <int> <chr>             <chr>       <chr>     <chr>    <dbl>
#>  1 8480280              1 https://assets.n… BOS         https://… G        6    
#>  2 8476999             35 https://assets.n… OTT         https://… G        6    
#>  3 8482661             30 https://assets.n… MIN         https://… G        5    
#>  4 8478872             70 https://assets.n… UTA         https://… G        5    
#>  5 8481611             52 https://assets.n… CAR         https://… G        5    
#>  6 8479979             29 https://assets.n… DAL         https://… G        3    
#>  7 8480193             80 https://assets.n… DET         https://… G        1    
#>  8 8484170             32 https://assets.n… MTL         https://… G        1    
#>  9 8481668             37 https://assets.n… PIT         https://… G        1    
#> 10 8482982             73 https://assets.n… CBJ         https://… G        1    
#> 11 8480193             80 https://assets.n… DET         https://… G        0.965
#> 12 8479979             29 https://assets.n… DAL         https://… G        0.952
#> 13 8476999             35 https://assets.n… OTT         https://… G        0.945
#> 14 8478009             30 https://assets.n… NYI         https://… G        0.940
#> 15 8478024             33 https://assets.n… ANA         https://… G        0.936
#> 16 8480193             80 https://assets.n… DET         https://… G        0.795
#> 17 8479979             29 https://assets.n… DAL         https://… G        1.000
#> 18 8476999             35 https://assets.n… OTT         https://… G        1.33 
#> 19 8478024             33 https://assets.n… ANA         https://… G        1.58 
#> 20 8478009             30 https://assets.n… NYI         https://… G        1.68 
#> # ℹ 11 more variables: first_name_default <chr>, last_name_default <chr>,
#> #   team_name_default <chr>, team_name_fr <chr>, first_name_cs <chr>,
#> #   first_name_fi <chr>, first_name_sk <chr>, last_name_fi <chr>,
#> #   category <chr>, last_name_cs <chr>, last_name_sk <chr>
# }
```
