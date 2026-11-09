# **Get ESPN NHL League Notes (core-v2)**

**Get ESPN NHL League Notes (core-v2)**

## Usage

``` r
espn_nhl_league_notes(...)
```

## Arguments

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per league note reference, or
an empty tibble when the endpoint is unavailable. **Note**: The NHL
league notes endpoint (`core_v2/notes`) currently returns `count=0` - an
empty tibble is returned gracefully:

|            |           |                                              |
|------------|-----------|----------------------------------------------|
| col_name   | types     | description                                  |
| ref        | character | `$ref` URL for the note object.              |
| note_id    | character | ESPN note id parsed from the `$ref` URL.     |
| count      | integer   | Total notes in the collection (typically 0). |
| page_count | integer   | Total pages in the collection.               |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  # NOTE: Returns empty tibble for NHL (ESPN does not populate this endpoint)
  try(espn_nhl_league_notes())
#> ! ESPN NHL league notes endpoint returned no items (count=0 is normal for NHL).
#> data frame with 0 columns and 0 rows
# }
```
