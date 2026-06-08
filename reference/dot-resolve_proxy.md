# Resolve the proxy to use for a request.

Resolution order: explicit `proxy` arg, then
`getOption("fastRhockey.proxy")`, else `NULL` (libcurl then reads
`http_proxy`/`https_proxy`/`no_proxy`).

## Usage

``` r
.resolve_proxy(proxy = NULL)
```

## Arguments

- proxy:

  `NULL`, a URL string, or a named list for
  [`httr2::req_proxy()`](https://httr2.r-lib.org/reference/req_proxy.html).

## Value

The resolved proxy value, or `NULL`.
