# **Get ESPN MCH / WCH Award (core-v2)**

**Get ESPN MCH / WCH Award (core-v2)**

## Usage

``` r
espn_mch_award(award_id, ...)

espn_wch_award(award_id, ...)
```

## Arguments

- award_id:

  ESPN award identifier (character or numeric). Use
  `espn_mch_awards()$award_id` to discover valid award ids.

- ...:

  Reserved for forward compatibility.

## Value

A one-row `fastRhockey_data` tibble with award metadata, or an empty
tibble when the endpoint is unavailable. **Note**: The individual award
endpoint (`core_v2/awards/{award_id}`) currently returns HTTP 500 for
all tested MCH award ids - an empty tibble is returned gracefully:

|              |           |                                                |
|--------------|-----------|------------------------------------------------|
| col_name     | types     | description                                    |
| award_id     | character | Award id (echoed from arg).                    |
| id           | character | ESPN award identifier (if returned).           |
| name         | character | Award name (if returned).                      |
| display_name | character | Award display name (if returned).              |
| description  | character | Award description (if returned).               |
| award_ref    | character | `$ref` URL for the award object (if returned). |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_award()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_award.md)
(columns inherited); a collection may be empty.

`espn_wch_award()` is the same wrapper for women's college hockey
(league slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  # NOTE: Returns empty tibble for MCH (ESPN returns HTTP 500 for this endpoint)
  try({
    awards <- espn_mch_awards()
    if (nrow(awards) > 0) espn_mch_award(award_id = awards$award_id[1])
  })
#> ── MENS-COLLEGE-HOCKEY Award data from ESPN core-v2 ─────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 17:18:10 UTC
#> # A tibble: 1 × 6
#>   award_id id    name  display_name description award_ref
#>   <chr>    <chr> <chr> <chr>        <chr>       <chr>    
#> 1 1        NA    NA    NA           NA          NA       
# }
# \donttest{
  # NOTE: Returns empty tibble for WCH (ESPN returns HTTP 500 for this endpoint)
  try({
    awards <- espn_wch_awards()
    if (nrow(awards) > 0) espn_wch_award(award_id = awards$award_id[1])
  })
#> ── WOMENS-COLLEGE-HOCKEY Award data from ESPN core-v2 ───── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 17:18:10 UTC
#> # A tibble: 1 × 6
#>   award_id id    name  display_name description award_ref
#>   <chr>    <chr> <chr> <chr>        <chr>       <chr>    
#> 1 1        NA    NA    NA           NA          NA       
# }
```
