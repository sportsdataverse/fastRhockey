# **Get ESPN NHL Standings**

**Get ESPN NHL Standings**

## Usage

``` r
espn_nhl_standings(season = most_recent_nhl_season(), ...)
```

## Arguments

- season:

  Season end-year (e.g. `2025`). Defaults to
  [`most_recent_nhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_nhl_season.md).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per team:

|                    |           |                                     |
|--------------------|-----------|-------------------------------------|
| col_name           | types     | description                         |
| group_name         | character | Group name (conference / division). |
| group_abbreviation | character | Group abbreviation.                 |
| team_id            | character | ESPN team identifier.               |
| team_name          | character | Team name.                          |
| team_abbreviation  | character | Team abbreviation.                  |
| team_display_name  | character | Team display name.                  |
| team_location      | character | Team location (city).               |
| team_logo          | character | Team logo URL.                      |
| season             | integer   | Season end-year (echoed from arg).  |
| ot_losses          | numeric   | Overtime losses.                    |
| games_played       | numeric   | Games played.                       |
| losses             | numeric   | Regulation losses.                  |
| points             | numeric   | Points.                             |
| wins               | numeric   | Wins.                               |
| points_for         | numeric   | Goals/points scored.                |
| points_against     | numeric   | Goals/points allowed.               |
| playoff_seed       | numeric   | Current playoff seed.               |
| streak             | numeric   | Current streak value.               |
| overall            | numeric   | Overall record stat.                |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
if (FALSE) { # interactive()
# \donttest{
  try(espn_nhl_standings(season = 2025))
# }
}
```
