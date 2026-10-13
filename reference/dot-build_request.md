# Build (not perform) the package's standard httr2 GET request.

The query, headers, proxy and timeout half of
[`.retry_request()`](https://fastRhockey.sportsdataverse.org/reference/dot-retry_request.md),
for callers that perform the request their own way (the European hockey
families add a POST body, a cookie jar or `req_options(resolve = ...)`
and perform through `.polite_perform()`).

## Usage

``` r
.build_request(
  url,
  params = list(),
  headers = NULL,
  timeout = 60,
  proxy = NULL
)
```

## Arguments

- url:

  Request URL.

- params:

  Named list of query parameters.

- headers:

  Named character vector / list of request headers.

- timeout:

  Seconds before timeout (default 60).

- proxy:

  `NULL`, URL string, or named list (see
  [`.resolve_proxy()`](https://fastRhockey.sportsdataverse.org/reference/dot-resolve_proxy.md)).

## Value

An httr2 request.
