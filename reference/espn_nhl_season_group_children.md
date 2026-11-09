# **Get ESPN NHL Season Group Children (core-v2)**

**Get ESPN NHL Season Group Children (core-v2)**

## Usage

``` r
espn_nhl_season_group_children(
  season = most_recent_nhl_season(),
  season_type = 2,
  group_id,
  ...
)
```

## Arguments

- season:

  Season end-year (e.g. `2026`). Defaults to
  [`most_recent_nhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_nhl_season.md).

- season_type:

  Season type code: `1`=pre-season, `2`=regular season, `3`=post-season.
  Defaults to `2`.

- group_id:

  ESPN group identifier (integer or character). Pass a conference-level
  `group_id` from
  [`espn_nhl_season_groups()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_groups.md)
  to retrieve its child divisions.

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per child group (division under
a conference). Conference-level groups typically have 2 division
children each:

|                |           |                                            |
|----------------|-----------|--------------------------------------------|
| col_name       | types     | description                                |
| ref            | character | `$ref` URL for the child group object.     |
| child_group_id | character | Child group id parsed from the `$ref` URL. |
| season         | integer   | Season year (echoed from arg).             |
| season_type    | integer   | Season type code (echoed from arg).        |
| group_id       | integer   | Parent group id (echoed from arg).         |
| count          | integer   | Total children in the collection.          |
| page_count     | integer   | Total pages in the collection.             |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try({
    grps <- espn_nhl_season_groups(season = 2026, season_type = 2)
    espn_nhl_season_group_children(season = 2026, season_type = 2,
                                    group_id = grps$group_id[1])
  })
#> ── NHL Season Group Children data from ESPN core-v2 ─────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 22:51:31 UTC
#> # A tibble: 2 × 7
#>   ref                child_group_id season season_type group_id count page_count
#>   <chr>              <chr>           <int>       <int>    <int> <int>      <int>
#> 1 http://sports.cor… 32               2026           2        7     2          1
#> 2 http://sports.cor… 33               2026           2        7     2          1
# }
```
