# **Get ESPN NHL Season Free Agents (core-v2)**

**Get ESPN NHL Season Free Agents (core-v2)**

## Usage

``` r
espn_nhl_season_freeagents(season = most_recent_nhl_season(), limit = 100, ...)
```

## Arguments

- season:

  Season end-year (e.g. `2026`). Defaults to
  [`most_recent_nhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_nhl_season.md).

- limit:

  Maximum number of free agents to return (default `100`).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per free-agent athlete
reference. **Note**: The NHL free-agents endpoint
(`seasons/{season}/freeagents`) currently returns `count=0` for all
tested seasons - the endpoint exists but ESPN does not populate it for
NHL. An empty tibble is returned gracefully in that case:

|            |           |                                                    |
|------------|-----------|----------------------------------------------------|
| col_name   | types     | description                                        |
| ref        | character | `$ref` URL for the athlete-in-season object.       |
| athlete_id | character | ESPN athlete id parsed from the `$ref` URL.        |
| season     | integer   | Season year (echoed from arg).                     |
| count      | integer   | Total free agents in the collection (typically 0). |
| page_count | integer   | Total pages in the collection.                     |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  # NOTE: Returns empty tibble for NHL (ESPN does not populate this endpoint)
  try(espn_nhl_season_freeagents(season = 2026))
#> data frame with 0 columns and 0 rows
# }
```
