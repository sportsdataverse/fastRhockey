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
#> ℹ Data updated: 2026-10-12 12:08:27 UTC
#> # A tibble: 20 × 15
#>         id sweater_number headshot          team_abbrev team_logo position value
#>      <int>          <int> <chr>             <chr>       <chr>     <chr>    <dbl>
#>  1 8480280              1 https://assets.n… BOS         https://… G        4    
#>  2 8476999             35 https://assets.n… OTT         https://… G        4    
#>  3 8482661             30 https://assets.n… MIN         https://… G        3    
#>  4 8478048             31 https://assets.n… NYR         https://… G        3    
#>  5 8478872             70 https://assets.n… UTA         https://… G        3    
#>  6 8479979             29 https://assets.n… DAL         https://… G        2    
#>  7 8484170             32 https://assets.n… MTL         https://… G        1    
#>  8 8480193             80 https://assets.n… DET         https://… G        1    
#>  9 8482193             33 https://assets.n… NYR         https://… G        1    
#> 10 8480981             30 https://assets.n… STL         https://… G        1    
#> 11 8480193             80 https://assets.n… DET         https://… G        0.974
#> 12 8479979             29 https://assets.n… DAL         https://… G        0.963
#> 13 8482193             33 https://assets.n… NYR         https://… G        0.946
#> 14 8483710             35 https://assets.n… TBL         https://… G        0.944
#> 15 8476999             35 https://assets.n… OTT         https://… G        0.942
#> 16 8480193             80 https://assets.n… DET         https://… G        0.660
#> 17 8479979             29 https://assets.n… DAL         https://… G        0.743
#> 18 8482193             33 https://assets.n… NYR         https://… G        1.20 
#> 19 8476999             35 https://assets.n… OTT         https://… G        1.34 
#> 20 8484170             32 https://assets.n… MTL         https://… G        1.46 
#> # ℹ 8 more variables: first_name_default <chr>, last_name_default <chr>,
#> #   team_name_default <chr>, team_name_fr <chr>, last_name_cs <chr>,
#> #   last_name_fi <chr>, last_name_sk <chr>, category <chr>
# }
```
