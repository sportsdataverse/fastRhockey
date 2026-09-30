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
#> ℹ Data updated: 2026-09-30 14:44:48 UTC
#> # A tibble: 17 × 15
#>         id sweater_number headshot          team_abbrev team_logo position value
#>      <int>          <int> <chr>             <chr>       <chr>     <chr>    <dbl>
#>  1 8474593             25 https://assets.n… FLA         https://… G        1    
#>  2 8482487             75 https://assets.n… MTL         https://… G        1    
#>  3 8480280              1 https://assets.n… BOS         https://… G        1    
#>  4 8480947             32 https://assets.n… VAN         https://… G        1    
#>  5 8479394             79 https://assets.n… VGK         https://… G        1    
#>  6 8474593             25 https://assets.n… FLA         https://… G        1    
#>  7 8480280              1 https://assets.n… BOS         https://… G        1    
#>  8 8474593             25 https://assets.n… FLA         https://… G        1    
#>  9 8480280              1 https://assets.n… BOS         https://… G        1    
#> 10 8483548             32 https://assets.n… CAR         https://… G        0.95 
#> 11 8482487             75 https://assets.n… MTL         https://… G        0.929
#> 12 8479394             79 https://assets.n… VGK         https://… G        0.923
#> 13 8474593             25 https://assets.n… FLA         https://… G        0    
#> 14 8480280              1 https://assets.n… BOS         https://… G        0    
#> 15 8483548             32 https://assets.n… CAR         https://… G        0.924
#> 16 8479394             79 https://assets.n… VGK         https://… G        2.00 
#> 17 8482487             75 https://assets.n… MTL         https://… G        2.01 
#> # ℹ 8 more variables: first_name_default <chr>, last_name_default <chr>,
#> #   last_name_cs <chr>, last_name_fi <chr>, last_name_sk <chr>,
#> #   last_name_sv <chr>, team_name_default <chr>, category <chr>
# }
```
