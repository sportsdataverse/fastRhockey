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
#> ℹ Data updated: 2026-04-08 06:16:54 UTC
#> # A tibble: 45 × 15
#>         id sweater_number headshot          team_abbrev team_logo position value
#>      <int>          <int> <chr>             <chr>       <chr>     <chr>    <dbl>
#>  1 8480802             71 https://assets.n… BUF         https://… C            5
#>  2 8480797             86 https://assets.n… CGY         https://… L            4
#>  3 8476399             20 https://assets.n… CGY         https://… L            4
#>  4 8481557             12 https://assets.n… MIN         https://… L            4
#>  5 8482093             24 https://assets.n… CAR         https://… C            4
#>  6 8477492             29 https://assets.n… COL         https://… C           55
#>  7 8476453             86 https://assets.n… TBL         https://… R           44
#>  8 8480039             88 https://assets.n… COL         https://… C           44
#>  9 8484258             70 https://assets.n… COL         https://… D           42
#> 10 8482655             90 https://assets.n… TBL         https://… D           41
#> # ℹ 35 more rows
#> # ℹ 8 more variables: first_name_default <chr>, last_name_default <chr>,
#> #   team_name_default <chr>, category <chr>, last_name_cs <chr>,
#> #   last_name_fi <chr>, last_name_sk <chr>, team_name_fr <chr>
# }
```
