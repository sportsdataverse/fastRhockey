# **NHL Where to Watch**

Returns streaming/broadcast availability information.

## Usage

``` r
nhl_where_to_watch()
```

## Value

Returns a list with streaming availability data.

## Examples

``` r
# \donttest{
try(nhl_where_to_watch())
#> 2026-11-25 20:24:04.210454: Error fetching where to watch: HTTP 404 Not Found from https://api-web.nhle.com/v1/where-to-watch
#> NULL
# }
```
