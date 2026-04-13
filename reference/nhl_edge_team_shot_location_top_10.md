# **NHL Edge Team Shot Location Top 10**

Returns the NHL Edge top-10 team shot-location leaderboard. Wraps
`https://api-web.nhle.com/v1/edge/team-shot-location-top-10/{position}/{category}/{sortBy}/...`.
When `season` is `NULL` (default) the `/now` endpoint is used to fetch
the current season.

## Usage

``` r
nhl_edge_team_shot_location_top_10(
  position,
  category,
  sort_by,
  season = NULL,
  game_type = 2
)
```

## Arguments

- position:

  Character position filter (e.g., `"F"`, `"D"`, `"all"`).

- category:

  Character shot-location category (e.g., `"high"`, `"medium"`,
  `"low"`).

- sort_by:

  Character metric to sort the leaderboard by (e.g., `"total"`,
  `"percentage"`).

- season:

  Optional 4-digit end-year (e.g., `2025` for the 2024-25 season), an
  8-character API season (e.g., `"20242025"`), or `NULL` (default) for
  the current season via the `/now` endpoint.

- game_type:

  Integer game type. 1 = preseason, 2 = regular season (default), 3 =
  playoffs.

## Value

A `fastRhockey_data` tibble with the top-10 team leaderboard, or `NULL`
on failure / empty response.

## Examples

``` r
# \donttest{
  try(nhl_edge_team_shot_location_top_10(
    position = "F",
    category = "high",
    sort_by = "total"
  ))
#> Request failed [404]. Retrying in 1 seconds...
#> Request failed [404]. Retrying in 1 seconds...
#> 2026-04-13 17:05:08.195827: Error fetching https://api-web.nhle.com/v1/edge/team-shot-location-top-10/F/high/total/now: The API returned an error
#> NULL
# }
```
