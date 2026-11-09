# **Get ESPN NHL Award (core-v2)**

**Get ESPN NHL Award (core-v2)**

## Usage

``` r
espn_nhl_award(award_id, ...)
```

## Arguments

- award_id:

  ESPN award identifier (character or numeric). Use
  `espn_nhl_awards()$award_id` to discover valid award ids.

- ...:

  Reserved for forward compatibility.

## Value

A one-row `fastRhockey_data` tibble with award metadata, or an empty
tibble when the endpoint is unavailable. **Note**: The individual award
endpoint (`core_v2/awards/{award_id}`) currently returns HTTP 500 for
all tested NHL award ids - an empty tibble is returned gracefully:

|              |           |                                                |
|--------------|-----------|------------------------------------------------|
| col_name     | types     | description                                    |
| award_id     | character | Award id (echoed from arg).                    |
| id           | character | ESPN award identifier (if returned).           |
| name         | character | Award name (if returned).                      |
| display_name | character | Award display name (if returned).              |
| description  | character | Award description (if returned).               |
| award_ref    | character | `$ref` URL for the award object (if returned). |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  # NOTE: Returns empty tibble for NHL (ESPN returns HTTP 500 for this endpoint)
  try({
    awards <- espn_nhl_awards()
    if (nrow(awards) > 0) espn_nhl_award(award_id = awards$award_id[1])
  })
#> ── NHL Award data from ESPN core-v2 ─────────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 11:49:41 UTC
#> # A tibble: 1 × 6
#>   award_id id    name  display_name description award_ref
#>   <chr>    <chr> <chr> <chr>        <chr>       <chr>    
#> 1 1        NA    NA    NA           NA          NA       
# }
```
