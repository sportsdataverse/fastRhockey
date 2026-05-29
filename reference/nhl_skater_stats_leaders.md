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

Returns a data frame with skater leaders.

## Examples

``` r
# \donttest{
  try(nhl_skater_stats_leaders())
#> ── NHL Skater Stats Leaders ─────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-05-29 14:43:20 UTC
#> # A tibble: 45 × 21
#>         id sweater_number headshot          team_abbrev team_logo position value
#>      <int>          <int> <chr>             <chr>       <chr>     <chr>    <dbl>
#>  1 8479353             21 https://assets.n… VGK         https://… C            3
#>  2 8478133             71 https://assets.n… MTL         https://… C            1
#>  3 8475188              3 https://assets.n… VGK         https://… D            1
#>  4 8478970              5 https://assets.n… CAR         https://… D            1
#>  5 8480835             18 https://assets.n… COL         https://… C            1
#>  6 8480817             19 https://assets.n… CAR         https://… D           12
#>  7 8478483             93 https://assets.n… VGK         https://… R           12
#>  8 8482122              7 https://assets.n… MIN         https://… D           11
#>  9 8480336             26 https://assets.n… CAR         https://… D           11
#> 10 8478133             71 https://assets.n… MTL         https://… C           11
#> # ℹ 35 more rows
#> # ℹ 14 more variables: first_name_default <chr>, last_name_default <chr>,
#> #   team_name_default <chr>, category <chr>, first_name_cs <chr>,
#> #   first_name_de <chr>, first_name_es <chr>, first_name_fi <chr>,
#> #   first_name_sk <chr>, first_name_sv <chr>, last_name_cs <chr>,
#> #   last_name_sk <chr>, last_name_fi <chr>, team_name_fr <chr>
# }
```
