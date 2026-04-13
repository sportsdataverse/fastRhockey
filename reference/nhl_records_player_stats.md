# **NHL Records - Player Career Stats**

Returns career player stats from the NHL Records API
(`https://records.nhl.com/site/api/player-stats`). Supports Cayenne
filtering and pagination.

## Usage

``` r
nhl_records_player_stats(cayenne_exp = NULL, limit = NULL, start = NULL)
```

## Arguments

- cayenne_exp:

  Optional Cayenne filter expression string (e.g. `"playerId=8478402"`).

- limit:

  Optional integer page size.

- start:

  Optional integer pagination offset.

## Value

A `fastRhockey_data` tibble of player career stats, or `NULL` on
failure.

## Examples

``` r
# \donttest{
  try(nhl_records_player_stats(cayenne_exp = "playerId=8478402"))
#> Request failed [400]. Retrying in 1 seconds...
#> Request failed [400]. Retrying in 3.9 seconds...
#> 2026-04-13 17:05:50.383166: Error fetching records resource 'player-stats': The API returned an error
#> NULL
# }
```
