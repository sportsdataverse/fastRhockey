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
  [`jsonlite::fromJSON()`](https://rdrr.io/pkg/jsonlite/man/fromJSON.html).

## Value

Parsed R object.
