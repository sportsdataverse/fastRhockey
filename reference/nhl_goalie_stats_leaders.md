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
#> ── NHL Goalie Stats Leaders ─────────────────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-01 23:58:44 UTC
#> # A tibble: 20 × 21
#>         id sweater_number headshot         team_abbrev team_logo position  value
#>      <int>          <int> <chr>            <chr>       <chr>     <chr>     <dbl>
#>  1 8481611             52 https://assets.… CAR         https://… G        10    
#>  2 8482661             30 https://assets.… MIN         https://… G         9    
#>  3 8480280              1 https://assets.… BOS         https://… G         7    
#>  4 8479979             29 https://assets.… DAL         https://… G         7    
#>  5 8478406             39 https://assets.… COL         https://… G         7    
#>  6 8479979             29 https://assets.… DAL         https://… G         3    
#>  7 8477465             35 https://assets.… EDM         https://… G         2    
#>  8 8484170             32 https://assets.… MTL         https://… G         2    
#>  9 8481668             37 https://assets.… PIT         https://… G         2    
#> 10 8481611             52 https://assets.… CAR         https://… G         2    
#> 11 8476999             35 https://assets.… OTT         https://… G         0.945
#> 12 8480193             80 https://assets.… DET         https://… G         0.940
#> 13 8484170             32 https://assets.… MTL         https://… G         0.932
#> 14 8481611             52 https://assets.… CAR         https://… G         0.929
#> 15 8480022             30 https://assets.… BOS         https://… G         0.929
#> 16 8476999             35 https://assets.… OTT         https://… G         1.33 
#> 17 8480193             80 https://assets.… DET         https://… G         1.40 
#> 18 8481611             52 https://assets.… CAR         https://… G         1.49 
#> 19 8479979             29 https://assets.… DAL         https://… G         1.63 
#> 20 8478048             31 https://assets.… NYR         https://… G         1.79 
#> # ℹ 14 more variables: first_name_default <chr>, first_name_cs <chr>,
#> #   first_name_fi <chr>, first_name_sk <chr>, last_name_default <chr>,
#> #   last_name_fi <chr>, team_name_default <chr>, category <chr>,
#> #   last_name_cs <chr>, last_name_sk <chr>, team_name_fr <chr>,
#> #   last_name_de <chr>, last_name_es <chr>, last_name_sv <chr>
# }
```
