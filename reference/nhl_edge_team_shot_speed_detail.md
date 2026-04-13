# **NHL Edge Team Shot Speed Detail**

Returns the NHL Edge shot-speed detail payload for a single team. Wraps
`https://api-web.nhle.com/v1/edge/team-shot-speed-detail/{teamId}/...`.
When `season` is `NULL` (default) the `/now` endpoint is used to fetch
the current season.

## Usage

``` r
nhl_edge_team_shot_speed_detail(team_id, season = NULL, game_type = 2)
```

## Arguments

- team_id:

  Integer NHL team ID (e.g., `10` for Toronto Maple Leafs).

- season:

  Optional 4-digit end-year (e.g., `2025` for the 2024-25 season), an
  8-character API season (e.g., `"20242025"`), or `NULL` (default) for
  the current season via the `/now` endpoint.

- game_type:

  Integer game type. 1 = preseason, 2 = regular season (default), 3 =
  playoffs.

## Value

A `fastRhockey_data` tibble of shot-speed metrics, or `NULL` on failure
/ empty response.

## Examples

``` r
# \donttest{
  try(nhl_edge_team_shot_speed_detail(team_id = 10))
#> ── NHL Edge Team Shot Speed Detail ──────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:05:08 UTC
#> # A tibble: 10 × 27
#>    game_center_link    game_date game_type is_home_team time_in_period player_id
#>    <chr>               <chr>         <int> <lgl>        <chr>              <int>
#>  1 /gamecenter/tor-vs… 2026-04-…         2 FALSE        00:55            8479026
#>  2 /gamecenter/tor-vs… 2026-01-…         2 FALSE        06:55            8479026
#>  3 /gamecenter/wsh-vs… 2026-04-…         2 TRUE         14:00            8479026
#>  4 /gamecenter/tor-vs… 2026-03-…         2 FALSE        10:56            8479026
#>  5 /gamecenter/tor-vs… 2025-11-…         2 FALSE        16:02            8481582
#>  6 /gamecenter/edm-vs… 2025-12-…         2 TRUE         04:52            8475714
#>  7 /gamecenter/pit-vs… 2025-12-…         2 TRUE         06:17            8479026
#>  8 /gamecenter/tor-vs… 2025-10-…         2 FALSE        19:24            8481582
#>  9 /gamecenter/tor-vs… 2026-04-…         2 FALSE        09:17            8481122
#> 10 /gamecenter/phi-vs… 2026-03-…         2 TRUE         01:23            8481122
#> # ℹ 21 more variables: player_slug <chr>, player_first_name_default <chr>,
#> #   player_last_name_default <chr>, player_last_name_cs <chr>,
#> #   player_last_name_fi <chr>, player_last_name_sk <chr>,
#> #   shot_speed_imperial <dbl>, shot_speed_metric <dbl>,
#> #   period_descriptor_number <int>, period_descriptor_period_type <chr>,
#> #   period_descriptor_max_regulation_periods <int>,
#> #   home_team_common_name_default <chr>, …
# }
```
