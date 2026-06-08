# Response body parsed from JSON.

Response body parsed from JSON.

## Usage

``` r
.resp_json(resp, ...)
```

## Arguments

- resp:

  An httr2 response.

- ...:

  Passed to
  [`jsonlite::fromJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html).

## Value

Parsed R object.
