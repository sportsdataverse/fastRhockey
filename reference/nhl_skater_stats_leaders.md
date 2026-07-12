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
#> ℹ Data updated: 2026-07-12 18:42:20 UTC
#> # A tibble: 45 × 15
#>         id sweater_number headshot          team_abbrev team_logo position value
#>      <int>          <int> <chr>             <chr>       <chr>     <chr>    <dbl>
#>  1 8480802             71 https://assets.n… BUF         https://… C            5
#>  2 8480797             86 https://assets.n… CGY         https://… L            4
#>  3 8476399             20 https://assets.n… CGY         https://… L            4
#>  4 8481557             12 https://assets.n… MIN         https://… L            4
#>  5 8482093             24 https://assets.n… CAR         https://… R            4
#>  6 8477492             29 https://assets.n… COL         https://… C           57
#>  7 8480039             88 https://assets.n… COL         https://… C           47
#>  8 8476453             86 https://assets.n… TBL         https://… R           43
#>  9 8484258             70 https://assets.n… COL         https://… D           43
#> 10 8476312             42 https://assets.n… COL         https://… D           42
#> # ℹ 35 more rows
#> # ℹ 8 more variables: first_name_default <chr>, last_name_default <chr>,
#> #   team_name_default <chr>, category <chr>, last_name_cs <chr>,
#> #   last_name_sk <chr>, last_name_fi <chr>, team_name_fr <chr>
# }
```
