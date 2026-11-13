# **Get ESPN NHL Season Group (core-v2)**

**Get ESPN NHL Season Group (core-v2)**

## Usage

``` r
espn_nhl_season_group(
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
  [`espn_nhl_season_groups()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_groups.md)
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

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_season_group(season = 2026, season_type = 2, group_id = 7))
#> ── NHL Season Group data from ESPN core-v2 ──────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 20:16:50 UTC
#> # A tibble: 1 × 15
#>   season season_type group_id id    uid   name  abbreviation slug  is_conference
#>    <int>       <int>    <int> <chr> <chr> <chr> <chr>        <chr> <lgl>        
#> 1   2026           2        7 7     s:70… East… East         east… TRUE         
#> # ℹ 6 more variables: group_ref <chr>, season_ref <chr>, parent_ref <chr>,
#> #   children_ref <chr>, standings_ref <chr>, teams_ref <chr>
# }
```
