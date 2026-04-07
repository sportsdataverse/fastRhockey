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
#> Request failed [404]. Retrying in 3.3 seconds...
#> 2026-04-07 04:54:31.899718: Error fetching where to watch: The API returned an error
#> NULL
# }
```
