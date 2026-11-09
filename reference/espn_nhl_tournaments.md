# **Get ESPN NHL Tournaments (core-v2)**

**Get ESPN NHL Tournaments (core-v2)**

## Usage

``` r
espn_nhl_tournaments(limit = 100, ...)
```

## Arguments

- limit:

  Maximum number of tournaments to return (default `100`).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per tournament reference. The
NHL tournaments endpoint is sparsely populated (typically 1 entry):

|               |           |                                                |
|---------------|-----------|------------------------------------------------|
| col_name      | types     | description                                    |
| ref           | character | `$ref` URL for the tournament object.          |
| tournament_id | character | ESPN tournament id parsed from the `$ref` URL. |
| count         | integer   | Total tournaments in the collection.           |
| page_count    | integer   | Total pages in the collection.                 |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_tournaments())
#> ── NHL Tournaments data from ESPN core-v2 ───────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 17:19:44 UTC
#> # A tibble: 1 × 4
#>   ref                                             tournament_id count page_count
#>   <chr>                                           <chr>         <int>      <int>
#> 1 http://sports.core.api.espn.com/v2/sports/hock… 1                 1          1
# }
```
