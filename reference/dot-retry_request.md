# Perform an HTTP GET with retries (httr2).

Replacement for the legacy httr RETRY-based GET. Non-2xx responses do
NOT throw (`req_error(is_error = ~ FALSE)`) so callers inspect status
via `check_status()`. Proxy resolved via
[`.resolve_proxy()`](https://fastRhockey.sportsdataverse.org/reference/dot-resolve_proxy.md);
accepts a URL string or a named list spread into
[`httr2::req_proxy()`](https://httr2.r-lib.org/reference/req_proxy.html).

## Usage

``` r
.retry_request(
  url,
  params = list(),
  headers = NULL,
  timeout = 60,
  proxy = NULL,
  max_tries = 3
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

- max_tries:

  Maximum attempts (default 3).

## Value

An httr2 response object.
