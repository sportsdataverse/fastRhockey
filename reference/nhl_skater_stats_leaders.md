# **NHL Skater Stats Leaders**

Returns league-wide skater statistical leaders for a given season and
game type. Supports multiple stat categories.

## Usage

``` r
nhl_skater_stats_leaders(
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

  Character vector of stat categories (e.g., "goals", "assists",
  "points", "plusMinus", "penaltyMins"). If NULL, returns all available
  categories.

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
| headshot           | character | URL to the player headshot image.   |
| team_abbrev        | character | Team abbreviation.                  |
| team_logo          | character | URL to the team logo image.         |
| position           | character | Player position.                    |
| value              | numeric   | Statistical value for the category. |
| first_name_default | character | Player first name (default).        |
| last_name_default  | character | Player last name (default).         |
| team_name_default  | character | Team name (default).                |
| category           | character | Stat leader category.               |
| first_name_cs      | character | Player first name (Czech).          |
| first_name_de      | character | Player first name (German).         |
| first_name_es      | character | Player first name (Spanish).        |
| first_name_fi      | character | Player first name (Finnish).        |
| first_name_sk      | character | Player first name (Slovak).         |
| first_name_sv      | character | Player first name (Swedish).        |
| last_name_cs       | character | Player last name (Czech).           |
| last_name_sk       | character | Player last name (Slovak).          |
| last_name_fi       | character | Player last name (Finnish).         |
| team_name_fr       | character | Team name (French).                 |

## Examples

``` r
# \donttest{
  try(nhl_skater_stats_leaders())
#> ── NHL Skater Stats Leaders ─────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-09-30 14:45:25 UTC
#> # A tibble: 39 × 17
#>         id sweater_number headshot          team_abbrev team_logo position value
#>      <int>          <int> <chr>             <chr>       <chr>     <chr>    <dbl>
#>  1 8481032             47 https://assets.n… VAN         https://… L            1
#>  2 8480029             26 https://assets.n… EDM         https://… L            3
#>  3 8476981             17 https://assets.n… MTL         https://… R            2
#>  4 8476854             27 https://assets.n… BOS         https://… D            2
#>  5 8478840             17 https://assets.n… BOS         https://… D            2
#>  6 8480355             47 https://assets.n… BOS         https://… C            2
#>  7 8480803              2 https://assets.n… EDM         https://… D            2
#>  8 8478403              9 https://assets.n… VGK         https://… C            2
#>  9 8478402             97 https://assets.n… EDM         https://… C            2
#> 10 8479425             17 https://assets.n… VAN         https://… D            2
#> # ℹ 29 more rows
#> # ℹ 10 more variables: first_name_default <chr>, last_name_default <chr>,
#> #   team_name_default <chr>, category <chr>, first_name_cs <chr>,
#> #   first_name_de <chr>, first_name_es <chr>, first_name_fi <chr>,
#> #   first_name_sk <chr>, first_name_sv <chr>
# }
```
