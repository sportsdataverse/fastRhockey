# **NHL Stats API — Players Listing**

Returns the player listing from the NHL Stats REST API
(`https://api.nhle.com/stats/rest/{lang}/players`). This is a full
player roster dump; use `cayenne_exp` to filter (e.g.,
`"playerId=8478402"`).

## Usage

``` r
nhl_stats_players(lang = "en", limit = 100, start = 0, cayenne_exp = NULL)
```

## Arguments

- lang:

  Character language code. Default `"en"`.

- limit:

  Integer maximum number of results. Default 100.

- start:

  Integer pagination start index. Default 0.

- cayenne_exp:

  Optional Cayenne filter expression string passed via the `cayenneExp`
  query parameter.

## Value

A `fastRhockey_data` tibble of players, or `NULL` on failure.

## Examples

``` r
# \donttest{
  try(nhl_stats_players())
#> 2026-04-13 17:06:02.136127: No players data (try passing a `cayenne_exp` filter)
#> NULL
# }
```
