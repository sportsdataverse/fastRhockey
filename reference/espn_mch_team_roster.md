# **Get ESPN MCH / WCH Team Roster**

**Get ESPN MCH / WCH Team Roster**

## Usage

``` r
espn_mch_team_roster(team_id, limit = 500, ...)

espn_wch_team_roster(team_id, limit = 500, ...)
```

## Arguments

- team_id:

  ESPN team identifier (character or numeric, e.g. `"104"` for
  Pittsburgh Penguins).

- limit:

  Maximum number of athletes (ESPN `limit`; default `500`, as
  sportsdataverse-py).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per player:

|                              |           |                                     |
|------------------------------|-----------|-------------------------------------|
| col_name                     | types     | description                         |
| position_group               | character | Position group name (e.g. Centers). |
| id                           | character | ESPN player identifier.             |
| uid                          | character | ESPN player uid.                    |
| guid                         | character | Player global unique identifier.    |
| alternate_id                 | character | Alternate player identifier.        |
| alternate_ids_sdr            | character | Alternate ids sdr.                  |
| first_name                   | character | First name.                         |
| last_name                    | character | Last name.                          |
| full_name                    | character | Full name.                          |
| display_name                 | character | Display name.                       |
| short_name                   | character | Short name.                         |
| weight                       | numeric   | Weight (lbs).                       |
| display_weight               | character | Display weight string.              |
| height                       | numeric   | Height (inches).                    |
| display_height               | character | Display height string.              |
| age                          | integer   | Player age.                         |
| date_of_birth                | character | Date of birth.                      |
| slug                         | character | URL slug.                           |
| jersey                       | character | Jersey number.                      |
| headshot_href                | character | Headshot image URL.                 |
| headshot_alt                 | character | Headshot alt text.                  |
| hand_type                    | character | Shooting/catching hand type.        |
| hand_abbreviation            | character | Hand abbreviation.                  |
| hand_display_value           | character | Hand display value.                 |
| position_id                  | character | Position identifier.                |
| position_name                | character | Position name.                      |
| position_display_name        | character | Position display name.              |
| position_abbreviation        | character | Position abbreviation.              |
| position_leaf                | logical   | Whether position is a leaf node.    |
| position_parent_id           | character | Parent position identifier.         |
| position_parent_name         | character | Parent position name.               |
| position_parent_display_name | character | Parent position display name.       |
| position_parent_abbreviation | character | Parent position abbreviation.       |
| position_parent_leaf         | logical   | Whether parent position is leaf.    |
| experience_years             | integer   | Experience years.                   |
| debut_year                   | numeric   | Debut year.                         |
| status_id                    | character | Status identifier.                  |
| status_name                  | character | Status name.                        |
| status_type                  | character | Status type.                        |
| status_abbreviation          | character | Status abbreviation.                |
| birth_place_city             | character | Birth place city.                   |
| birth_place_state            | character | Birth place state.                  |
| birth_place_country          | character | Birth place country.                |
| birth_place_display_text     | character | Birth place display text.           |
| birth_country_abbreviation   | character | Birth country abbreviation.         |
| college_id                   | character | College identifier.                 |
| college_guid                 | character | College guid.                       |
| college_mascot               | character | College mascot.                     |
| college_name                 | character | College name.                       |
| college_short_name           | character | College short name.                 |
| college_abbrev               | character | College abbreviation.               |
| college_logos                | character | College logo URLs (pipe-delimited). |
| team_id                      | character | ESPN team identifier (from arg).    |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_team_roster.md);
ESPN answered an empty collection for every captured MCH body (2026-10),
so the columns below are the NHL helper's and unverified for this
league.

`espn_wch_team_roster()` is the same wrapper for women's college hockey
(league slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_mch_team_roster(team_id = "104"))
#> ✖ 2026-11-09 22:50:54.339445: Invalid arguments or no ESPN mens-college-hockey team roster data available!
#> ✖ Args: league = "mens-college-hockey", team_id = "104", limit = 500
#> ✖ Error: arguments imply differing number of rows: 1, 0
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  try(espn_wch_team_roster(team_id = "2137"))
#> data frame with 0 columns and 0 rows
# }
```
