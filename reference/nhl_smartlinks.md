# **NHL Smart Links**

Returns NHL "smart link" routing metadata from the NHL web service
endpoint `smartlinks`. Used by NHL.com's link router to resolve short
handles into destination URLs.

## Usage

``` r
nhl_smartlinks(handle = NULL)
```

## Arguments

- handle:

  Optional character handle. If provided, the lookup is narrowed to a
  single smart link via the `handle` query parameter.

## Value

Returns a list with smart link routing data.

## Examples

``` r
# \donttest{
  try(nhl_smartlinks())
#> list()
# }
```
