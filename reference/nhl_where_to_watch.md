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
#> Request failed [404]. Retrying in 1 seconds...
#> Request failed [404]. Retrying in 2.7 seconds...
#> 2026-04-13 17:06:12.501202: Error fetching where to watch: The API returned an error
#> NULL
# }
```
