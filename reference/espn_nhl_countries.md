# **Get ESPN NHL Countries (core-v2)**

**Get ESPN NHL Countries (core-v2)**

## Usage

``` r
espn_nhl_countries(limit = 200, ...)
```

## Arguments

- limit:

  Maximum number of countries to return (default `200`).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per country reference, or an
empty tibble when the endpoint is unpopulated. **Note**: The NHL
countries endpoint (`leagues/nhl/countries`) returns `count=0` - ESPN
does not populate this collection for the NHL. An empty tibble is
returned gracefully with a warning:

|            |           |                                             |
|------------|-----------|---------------------------------------------|
| col_name   | types     | description                                 |
| ref        | character | `$ref` URL for the country object.          |
| country_id | character | ESPN country id parsed from the `$ref` URL. |
| count      | integer   | Total countries (typically `0` for NHL).    |
| page_count | integer   | Total pages in the collection.              |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  # NOTE: Returns empty tibble for NHL (ESPN does not populate this endpoint)
  try(espn_nhl_countries())
#> ! 2026-11-09 19:34:24.387184: No country items returned for nhl (count=0 is normal for NHL).
#> data frame with 0 columns and 0 rows
# }
```
