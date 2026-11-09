# **Get ESPN MCH / WCH Standings (core-v2)**

**Get ESPN MCH / WCH Standings (core-v2)**

## Usage

``` r
espn_mch_standings_core(...)

espn_wch_standings_core(...)
```

## Arguments

- ...:

  Passed through to
  [`.retry_request()`](https://fastRhockey.sportsdataverse.org/reference/dot-retry_request.md)
  (e.g. `proxy =`).

## Value

A `fastRhockey_data` tibble with one row per standings item:

|              |           |                                     |
|--------------|-----------|-------------------------------------|
| col_name     | types     | description                         |
| ref          | character | `$ref` URL of the standings object. |
| standings_id | character | Trailing id of the `$ref`.          |

## Details

League slug `mens-college-hockey`. Core-v2 league-level `standings`;
ESPN answers with a bare `$ref` pointer to the current season's group
standings, returned as one `ref` row. Mirrors sportsdataverse-py
`espn_mch_standings_core()`, which takes no arguments; use
[`espn_mch_standings()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_standings.md)
for the per-team table.

`espn_wch_standings_core()` is the same wrapper for women's college
hockey (league slug `womens-college-hockey`): same arguments, same
columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_mch_standings_core())
#> ── MENS-COLLEGE-HOCKEY Standings data from ESPN core-v2 ─── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 17:18:48 UTC
#> # A tibble: 1 × 2
#>   ref                                                               standings_id
#>   <chr>                                                             <chr>       
#> 1 http://sports.core.api.espn.com/v2/sports/hockey/leagues/mens-co… standings   
# }
# \donttest{
  try(espn_wch_standings_core())
#> ── WOMENS-COLLEGE-HOCKEY Standings data from ESPN core-v2 ──────────────────────
#> ℹ Data updated: 2026-11-09 17:18:49 UTC
#> # A tibble: 1 × 2
#>   ref                                                               standings_id
#>   <chr>                                                             <chr>       
#> 1 http://sports.core.api.espn.com/v2/sports/hockey/leagues/womens-… standings   
# }
```
