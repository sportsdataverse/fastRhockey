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
2025-26). Same ESPN helper and columns as
[`espn_nhl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_team_roster.md).
College rosters are flat (not grouped by position), so `position_group`
is `NA`, and any field ESPN leaves out for a player is `NA`; parsed
against a captured MCH roster (team 104, 2026-11-24).

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
#> ── MENS-COLLEGE-HOCKEY Team Roster data from ESPN.com ───── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-25 04:39:33 UTC
#> # A tibble: 14 × 53
#>    position_group id      uid    guid  alternate_id alternate_ids_sdr first_name
#>    <chr>          <chr>   <chr>  <chr> <chr>        <chr>             <chr>     
#>  1 NA             5136656 s:70~… NA    5136656      5136656           Tristan   
#>  2 NA             5216936 s:70~… NA    5216936      5216936           Kamil     
#>  3 NA             5206627 s:70~… NA    5206627      5206627           Mathieu   
#>  4 NA             5188559 s:70~… NA    5188559      5188559           Aiden     
#>  5 NA             4997117 s:70~… NA    4997117      4997117           Brehdan   
#>  6 NA             5206622 s:70~… NA    5206622      5206622           Mick      
#>  7 NA             5275393 s:70~… NA    5275393      5275393           Billy     
#>  8 NA             5206624 s:70~… NA    5206624      5206624           Jack      
#>  9 NA             5206623 s:70~… NA    5206623      5206623           Doug      
#> 10 NA             5188526 s:70~… NA    5188526      5188526           Jack      
#> 11 NA             4996314 s:70~… NA    4996314      4996314           Jack      
#> 12 NA             5206626 s:70~… NA    5206626      5206626           Max       
#> 13 NA             5136661 s:70~… NA    5136661      5136661           Jack      
#> 14 NA             5275394 s:70~… NA    5275394      5275394           Nick      
#> # ℹ 46 more variables: last_name <chr>, full_name <chr>, display_name <chr>,
#> #   short_name <chr>, weight <dbl>, display_weight <chr>, height <dbl>,
#> #   display_height <chr>, age <int>, date_of_birth <chr>, slug <chr>,
#> #   jersey <chr>, headshot_href <chr>, headshot_alt <chr>, hand_type <chr>,
#> #   hand_abbreviation <chr>, hand_display_value <chr>, position_id <chr>,
#> #   position_name <chr>, position_display_name <chr>,
#> #   position_abbreviation <chr>, position_leaf <lgl>, …
# }
# \donttest{
  try(espn_wch_team_roster(team_id = "2137"))
#> data frame with 0 columns and 0 rows
# }
```
