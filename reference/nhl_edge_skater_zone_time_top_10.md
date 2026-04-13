# **NHL Edge Skater Zone Time Top 10**

Returns the NHL Edge top-10 zone-time leaderboard for a given
`positions`, `strength`, and `sort_by` combination. Wraps
`https://api-web.nhle.com/v1/edge/skater-zone-time-top-10/{positions}/{strength}/{sortBy}/...`.
When `season` is `NULL` (default) the `/now` endpoint is used to fetch
the current season.

## Usage

``` r
nhl_edge_skater_zone_time_top_10(
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
  try(nhl_edge_skater_zone_time_top_10(
      positions = "F",
      strength = "all",
      sort_by = "total"
  ))
#> Request failed [500]. Retrying in 1 seconds...
#> Request failed [500]. Retrying in 1.3 seconds...
#> 2026-04-13 17:05:03.780751: Error fetching https://api-web.nhle.com/v1/edge/skater-zone-time-top-10/F/all/total/now: The API returned an error
#> NULL
# }
```
