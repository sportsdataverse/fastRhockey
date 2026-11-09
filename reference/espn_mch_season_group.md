# **Get ESPN MCH / WCH Season Group (core-v2)**

**Get ESPN MCH / WCH Season Group (core-v2)**

## Usage

``` r
espn_mch_season_group(
  season = most_recent_nhl_season(),
  season_type = 2,
  group_id,
  ...
)

espn_wch_season_group(
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

  Season type code: `1`=pre-season, `2`=regular season, `3`=post-season,
  `4`=off-season. Defaults to `2`.

- group_id:

  ESPN group identifier (integer or character). Use
  [`espn_mch_season_groups()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_season_groups.md)
  to discover valid ids for a season/type.

- ...:

  Reserved for forward compatibility.

## Value

A one-row `fastRhockey_data` tibble with group metadata:

|               |           |                                                    |
|---------------|-----------|----------------------------------------------------|
| col_name      | types     | description                                        |
| season        | integer   | Season year (echoed from arg).                     |
| season_type   | integer   | Season type code (echoed from arg).                |
| group_id      | integer   | Group id (echoed from arg).                        |
| id            | character | ESPN group identifier.                             |
| uid           | character | Group uid string.                                  |
| name          | character | Group name (e.g. "Eastern Conference").            |
| abbreviation  | character | Group abbreviation (e.g. "East").                  |
| slug          | character | Group slug.                                        |
| is_conference | logical   | Whether this group is a conference (vs. division). |
| group_ref     | character | `$ref` URL for this group object.                  |
| season_ref    | character | `$ref` URL for the group's season.                 |
| parent_ref    | character | `$ref` URL for the parent group (if any).          |
| children_ref  | character | `$ref` URL for child groups (divisions).           |
| standings_ref | character | `$ref` URL for group standings.                    |
| teams_ref     | character | `$ref` URL for the teams in this group.            |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_season_group()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_group.md)
(columns inherited); a collection may be empty.

`espn_wch_season_group()` is the same wrapper for women's college hockey
(league slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_mch_season_group(season = 2026, season_type = 2, group_id = 7))
#> ✖ 2026-11-09 19:34:04.145402: Invalid arguments or no ESPN mens-college-hockey season group 7 for 2026 type 2 available!
#> ✖ Args: league = "mens-college-hockey", season = 2026, season_type = 2, group_id = 7
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  try(espn_wch_season_group(season = 2026, season_type = 2, group_id = 7))
#> ✖ 2026-11-09 19:34:04.170039: Invalid arguments or no ESPN womens-college-hockey season group 7 for 2026 type 2 available!
#> ✖ Args: league = "womens-college-hockey", season = 2026, season_type = 2, group_id = 7
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
```
