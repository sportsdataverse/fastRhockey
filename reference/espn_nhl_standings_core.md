# **Get ESPN NHL Standings (core-v2)**

**Get ESPN NHL Standings (core-v2)**

## Usage

``` r
espn_nhl_standings_core(
  season = most_recent_nhl_season(),
  season_type = 2,
  group_id = 9,
  ...
)
```

## Arguments

- season:

  Season end-year (e.g. `2026`). Defaults to
  [`most_recent_nhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_nhl_season.md).

- season_type:

  Season type code: `1`=pre-season, `2`=regular season (default),
  `3`=post-season.

- group_id:

  Standings group identifier. Defaults to `9`, which is the verified
  all-teams rollup group for NHL on core-v2.

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per team per standings group.
Key columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| group_id_v | character | Standings group identifier. |
| group_name | character | Group name (e.g. "overall"). |
| group_display_name | character | Group display name. |
| team_id | character | ESPN team identifier (parsed from `$ref`). |
| team_ref | character | `$ref` URL for the team-in-season. |
| record_type | character | Record type used (always "total"). |
| record_summary | character | Record summary string (e.g. "53-22-7"). |
| season | integer | Season year (echoed from arg). |
| season_type | integer | Season type code (echoed from arg). |
| ot_losses | numeric | Overtime losses (and other stat columns dynamically). |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
if (FALSE) { # interactive()
# \donttest{
  try(espn_nhl_standings_core(season = 2026, season_type = 2))
# }
}
```
