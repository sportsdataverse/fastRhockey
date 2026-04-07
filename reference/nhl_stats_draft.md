# **NHL Stats API — Draft Summaries**

Queries the NHL Stats REST API for draft-year summaries (draft year and
number of rounds). Optionally filter to a specific draft year via
`draft_year`.

## Usage

``` r
nhl_stats_draft(draft_year = NULL, limit = 100, start = 0, lang = "en")
```

## Arguments

- draft_year:

  Integer draft year (e.g., 2024). If NULL, returns all draft years
  (1963-present).

- limit:

  Integer maximum number of results. Default 100.

- start:

  Integer start index for pagination. Default 0.

- lang:

  Character language code. Default "en".

## Value

Returns a data frame with columns: id, draft_year, rounds.

## Examples

``` r
# \donttest{
  try(nhl_stats_draft())
#> ── NHL Stats Draft ──────────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-07 13:28:47 UTC
#> # A tibble: 63 × 3
#>       id draft_year rounds
#>    <int>      <int>  <int>
#>  1     1       1989     12
#>  2     2       1975     18
#>  3     3       2008      7
#>  4     4       2013      7
#>  5     5       2006      7
#>  6     6       1994     11
#>  7     7       1982     12
#>  8     8       1968      3
#>  9     9       2018      7
#> 10    10       1999      9
#> # ℹ 53 more rows
  try(nhl_stats_draft(draft_year = 2024))
#> ── NHL Stats Draft ──────────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-07 13:28:47 UTC
#> # A tibble: 1 × 3
#>      id draft_year rounds
#>   <int>      <int>  <int>
#> 1    63       2024      7
# }
```
