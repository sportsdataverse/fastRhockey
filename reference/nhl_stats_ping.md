# **NHL Stats API — Ping**

Health-check endpoint for the NHL Stats REST API
(`https://api.nhle.com/stats/rest/ping`). This endpoint is non-tabular
and is not language-scoped: the raw parsed payload is returned as-is
rather than wrapped in a `fastRhockey_data` tibble.

## Usage

``` r
nhl_stats_ping()
```

## Value

A parsed list with the health-check payload, or `NULL` on failure.

## Examples

``` r
# \donttest{
  try(nhl_stats_ping())
#> named list()
# }
```
