# **Get ESPN NHL Player Info**

**Get ESPN NHL Player Info**

## Usage

``` r
espn_nhl_player_info(athlete_id, ...)
```

## Arguments

- athlete_id:

  ESPN athlete identifier (character or numeric, e.g. `"5149125"`).
  Obtain via
  [`espn_nhl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_team_roster.md).

- ...:

  Reserved for forward compatibility.

## Value

A one-row `fastRhockey_data` tibble with athlete metadata from the
`site_v2` athletes endpoint, or an empty
[`data.frame()`](https://rdrr.io/r/base/data.frame.html) when ESPN
returns HTTP 404. As of 2026-06-08, ESPN's `site_v2` NHL `athletes/{id}`
endpoint returns 404; use
[`espn_nhl_player_core()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_player_core.md)
for athlete metadata instead.

|                   |           |                                            |
|-------------------|-----------|--------------------------------------------|
| col_name          | types     | description                                |
| athlete_id        | character | ESPN athlete identifier (echoed from arg). |
| athlete_espn_id   | character | ESPN athlete id from payload.              |
| uid               | character | Athlete uid string.                        |
| guid              | character | Athlete global unique identifier.          |
| display_name      | character | Player display name.                       |
| short_name        | character | Player short name.                         |
| full_name         | character | Player full name.                          |
| first_name        | character | First name.                                |
| last_name         | character | Last name.                                 |
| jersey            | character | Jersey number.                             |
| slug              | character | URL slug.                                  |
| position          | character | Position abbreviation.                     |
| position_name     | character | Position full name.                        |
| age               | integer   | Current age.                               |
| date_of_birth     | character | Date of birth (ISO 8601).                  |
| debut_year        | integer   | Year of NHL debut.                         |
| weight            | character | Formatted weight string.                   |
| height            | character | Formatted height string.                   |
| team_id           | character | ESPN team identifier.                      |
| team_display_name | character | Team display name.                         |
| team_abbreviation | character | Team abbreviation.                         |
| headshot_href     | character | Player headshot image URL.                 |
| active            | logical   | Whether athlete is currently active.       |
| status            | character | Athlete status description.                |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_player_info(athlete_id = "5149125"))
#> ! ESPN site_v2 athlete info (athletes/[id]) returns HTTP 404 for NHL; use espn_nhl_player_core() instead.
#> data frame with 0 columns and 0 rows
# }
```
