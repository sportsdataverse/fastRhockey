# Report an API error via cli (optional brace-interpolated hint + args).

Report an API error via cli (optional brace-interpolated hint + args).

## Usage

``` r
.report_api_error(e, hint = NULL, args = list())
```

## Arguments

- e:

  Captured condition.

- hint:

  Optional glue-style template evaluated against `args`.

- args:

  Named list of caller args (see
  [`.capture_args()`](https://fastRhockey.sportsdataverse.org/reference/dot-capture_args.md)).

## Value

invisible(NULL)
