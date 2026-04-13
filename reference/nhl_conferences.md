# **NHL Conferences**

Returns a table of current NHL conferences derived from standings data.

The original NHL Stats API conferences endpoint is no longer available.
This function now extracts conference information from the standings
endpoint at `api-web.nhle.com`.

## Usage

``` r
nhl_conferences(date = NULL)
```

## Arguments

- date:

  Character date in "YYYY-MM-DD" format. If NULL, returns current
  conferences.

## Value

Returns a data frame with columns:

- `conference_name` - conference name (e.g. "Eastern", "Western")

## Examples

``` r
# \donttest{
  try(nhl_conferences())
#> ── NHL Conferences from NHL.com ─────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:04:25 UTC
#> # A tibble: 2 × 1
#>   conference_name
#>   <chr>          
#> 1 Western        
#> 2 Eastern        
# }
```
