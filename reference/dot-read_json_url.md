# GET a JSON URL through [`.retry_request()`](https://fastRhockey.sportsdataverse.org/reference/dot-retry_request.md) and parse it like `jsonlite::read_json(url, simplifyVector = TRUE)`, but with the package's retry, proxy and timeout handling. A non-200 answer is an error.

GET a JSON URL through
[`.retry_request()`](https://fastRhockey.sportsdataverse.org/reference/dot-retry_request.md)
and parse it like `jsonlite::read_json(url, simplifyVector = TRUE)`, but
with the package's retry, proxy and timeout handling. A non-200 answer
is an error.

## Usage

``` r
.read_json_url(url)
```

## Arguments

- url:

  Request URL.

## Value

Parsed R object.
