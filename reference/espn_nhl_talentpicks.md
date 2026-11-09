# **Get ESPN NHL Talent Picks (core-v2)**

**Get ESPN NHL Talent Picks (core-v2)**

## Usage

``` r
espn_nhl_talentpicks(...)
```

## Arguments

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per talent pick reference, or
an empty tibble when the endpoint is unavailable. **Note**: The NHL
talent picks endpoint (`core_v2/talentpicks`) currently returns
`count=0`

- an empty tibble is returned gracefully:

|            |           |                                                     |
|------------|-----------|-----------------------------------------------------|
| col_name   | types     | description                                         |
| ref        | character | `$ref` URL for the talent pick object.              |
| pick_id    | character | ESPN pick id parsed from the `$ref` URL.            |
| count      | integer   | Total talent picks in the collection (typically 0). |
| page_count | integer   | Total pages in the collection.                      |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  # NOTE: Returns empty tibble for NHL (ESPN does not populate this endpoint)
  try(espn_nhl_talentpicks())
#> ! ESPN NHL talentpicks endpoint returned no items (count=0 is normal for NHL).
#> data frame with 0 columns and 0 rows
# }
```
