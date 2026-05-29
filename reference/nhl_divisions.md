# **NHL Divisions**

Returns information on NHL divisions derived from standings data.

The original NHL Stats API divisions endpoint is no longer available.
This function now extracts division information from the standings
endpoint at `api-web.nhle.com`.

## Usage

``` r
nhl_divisions(date = NULL)
```

## Arguments

- date:

  Character date in "YYYY-MM-DD" format. If NULL, returns current
  divisions.

## Value

A data frame (`fastRhockey_data`) with the following columns:

|                 |           |                                                  |
|-----------------|-----------|--------------------------------------------------|
| col_name        | types     | description                                      |
| division_name   | character | Division name (e.g. "Atlantic", "Metropolitan"). |
| division_abbrev | character | Division abbreviation.                           |
| conference_name | character | Parent conference name.                          |

## Examples

``` r
# \donttest{
   try(nhl_divisions())
#> ── NHL Divisions from NHL.com ───────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-05-29 17:43:27 UTC
#> # A tibble: 4 × 3
#>   division_name division_abbrev conference_name
#>   <chr>         <chr>           <chr>          
#> 1 Central       C               Western        
#> 2 Metropolitan  M               Eastern        
#> 3 Atlantic      A               Eastern        
#> 4 Pacific       P               Western        
# }
```
