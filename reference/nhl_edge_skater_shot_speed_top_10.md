# **NHL Edge Skater Shot Speed Top 10**

Returns the NHL Edge top-10 "hardest-shot" leaderboard for a given
`positions` and `sort_by` combination. Wraps
`https://api-web.nhle.com/v1/edge/skater-shot-speed-top-10/{positions}/{sortBy}/...`.
When `season` is `NULL` (default) the `/now` endpoint is used to fetch
the current season.

## Usage

``` r
nhl_edge_skater_shot_speed_top_10(
  positions,
  sort_by,
  season = NULL,
  game_type = 2
)
```

## Arguments

- positions:

  Character positions filter accepted by the Edge API (e.g., `"F"` for
  forwards, `"D"` for defensemen, or a combined filter).

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
  try(nhl_edge_skater_shot_speed_top_10(
      positions = "F",
      sort_by = "total"
  ))
#> Warning: `nhl_edge_skater_shot_speed_top_10()` was deprecated in fastRhockey 1.0.0.
#> ℹ The NHL has removed this Edge top-10 leaderboard endpoint; the function
#>   returns NULL.
#> 2026-06-12 22:25:21.629527: Error fetching https://api-web.nhle.com/v1/edge/skater-shot-speed-top-10/F/total/now: The API returned an error
#> NULL
# }
```
