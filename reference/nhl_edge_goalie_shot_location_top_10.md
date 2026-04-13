# **NHL Edge Goalie Shot Location Top 10**

Returns the NHL Edge top-10 goalie leaderboard for a given shot-location
`category` and `sort_by` combination. Wraps
`https://api-web.nhle.com/v1/edge/goalie-shot-location-top-10/{category}/{sortBy}/...`.
When `season` is `NULL` (default) the `/now` endpoint is used to fetch
the current season.

## Usage

``` r
nhl_edge_goalie_shot_location_top_10(
  category,
  sort_by,
  season = NULL,
  game_type = 2
)
```

## Arguments

- category:

  Character shot-location category accepted by the Edge API (e.g.,
  `"high"`, `"mid"`, `"low"`).

- sort_by:

  Character sort-by key accepted by the Edge API (e.g., `"savePctg"`).

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
  try(nhl_edge_goalie_shot_location_top_10(
      category = "high",
      sort_by = "savePctg"
  ))
#> Request failed [404]. Retrying in 1 seconds...
#> Request failed [404]. Retrying in 1.9 seconds...
#> 2026-04-13 17:04:43.86736: Error fetching https://api-web.nhle.com/v1/edge/goalie-shot-location-top-10/high/savePctg/now: The API returned an error
#> NULL
# }
```
