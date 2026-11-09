# **Get ESPN MCH / WCH Season Group Children (core-v2)**

**Get ESPN MCH / WCH Season Group Children (core-v2)**

## Usage

``` r
espn_mch_season_group_children(
  season = most_recent_nhl_season(),
  season_type = 2,
  group_id,
  ...
)

espn_wch_season_group_children(
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
  [`espn_mch_season_groups()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_season_groups.md)
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

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_season_group_children()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_group_children.md)
(columns inherited); a collection may be empty.

`espn_wch_season_group_children()` is the same wrapper for women's
college hockey (league slug `womens-college-hockey`): same arguments,
same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try({
    grps <- espn_mch_season_groups(season = 2026, season_type = 2)
    espn_mch_season_group_children(season = 2026, season_type = 2,
                                    group_id = grps$group_id[1])
  })
#> ! ESPN MENS-COLLEGE-HOCKEY season group 61 has no children for 2026 type 2.
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  try({
    grps <- espn_wch_season_groups(season = 2026, season_type = 2)
    espn_wch_season_group_children(season = 2026, season_type = 2,
                                    group_id = grps$group_id[1])
  })
#> ✖ 2026-11-09 11:49:28.304191: Invalid arguments or no ESPN womens-college-hockey season groups for 2026 type 2 available!
#> ✖ Args: league = "womens-college-hockey", season = 2026, season_type = 2
#> ✖ Error: replacement has 1 row, data has 0
#> ✖ 2026-11-09 11:49:28.334403: 
#> ✖ Args: league = "womens-college-hockey", season = 2026, season_type = 2, group_id = NULL
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
```
