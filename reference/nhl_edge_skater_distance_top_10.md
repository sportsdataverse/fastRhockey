# **NHL Edge Skater Distance Top 10**

Returns the NHL Edge top-10 "iron-man / mileage" skating distance
leaderboard for a given `positions`, `strength`, and `sort_by`
combination. Wraps
`https://api-web.nhle.com/v1/edge/skater-distance-top-10/{positions}/{strength}/{sortBy}/...`.
When `season` is `NULL` (default) the `/now` endpoint is used to fetch
the current season.

## Usage

``` r
nhl_edge_skater_distance_top_10(
  positions,
  strength,
  sort_by,
  season = NULL,
  game_type = 2
)
```

## Arguments

- positions:

  Character positions filter accepted by the Edge API (e.g., `"F"` for
  forwards, `"D"` for defensemen).

- strength:

  Character strength filter accepted by the Edge API (e.g., `"all"`,
  `"ev"`, `"pp"`, `"sh"`).

- sort_by:

  Character sort-by key accepted by the Edge API (e.g., `"total"`).

- season:

  Optional 4-digit end-year (e.g., `2025` for the 2024-25 season), an
  8-character API season (e.g., `"20242025"`), or `NULL` (default) for
  the current season via the `/now` endpoint.

- game_type:

  Integer game type. 1 = preseason, 2 = regular season (default), 3 =
  playoffs.

## Value

A `fastRhockey_data` tibble with the top-10 leaderboard, or `NULL` on
failure / empty response.

## Examples

``` r
# \donttest{
  try(nhl_edge_skater_distance_top_10(
      positions = "F",
      strength = "all",
      sort_by = "total"
  ))
#> ── NHL Edge Skater Distance Top 10 ──────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:04:45 UTC
#> # A tibble: 10 × 53
#>    player_slug             player_headshot player_position player_sweater_number
#>    <chr>                   <chr>           <chr>                           <int>
#>  1 connor-mcdavid-8478402  https://assets… C                                  97
#>  2 macklin-celebrini-8484… https://assets… C                                  71
#>  3 kyle-connor-8478398     https://assets… L                                  81
#>  4 kirill-kaprizov-8478864 https://assets… L                                  97
#>  5 martin-necas-8480039    https://assets… C                                  88
#>  6 mark-scheifele-8476460  https://assets… C                                  55
#>  7 nathan-mackinnon-84774… https://assets… C                                  29
#>  8 mathew-barzal-8478445   https://assets… C                                  13
#>  9 nick-suzuki-8480018     https://assets… C                                  14
#> 10 mika-zibanejad-8476459  https://assets… C                                  93
#> # ℹ 49 more variables: player_first_name_default <chr>,
#> #   player_last_name_default <chr>, player_last_name_cs <chr>,
#> #   player_last_name_sk <chr>, player_team_abbrev <chr>,
#> #   player_team_slug <chr>, player_team_common_name_default <chr>,
#> #   player_team_place_name_with_preposition_default <chr>,
#> #   player_team_place_name_with_preposition_fr <chr>,
#> #   player_team_team_logo_light <chr>, player_team_team_logo_dark <chr>, …
# }
```
