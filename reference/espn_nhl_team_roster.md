# **Get ESPN NHL Team Roster**

**Get ESPN NHL Team Roster**

## Usage

``` r
espn_nhl_team_roster(team_id, ...)
```

## Arguments

- team_id:

  ESPN team identifier (character or numeric, e.g. `"4"` for Pittsburgh
  Penguins).

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

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_team_roster(team_id = "4"))
#> ── NHL Team Roster data from ESPN.com ───────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 19:35:28 UTC
#> # A tibble: 25 × 53
#>    position_group id      uid    guid  alternate_id alternate_ids_sdr first_name
#>    <chr>          <chr>   <chr>  <chr> <chr>        <chr>             <chr>     
#>  1 Centers        5149125 s:70~… 81dd… 5149125      5149125           Connor    
#>  2 Centers        3115033 s:70~… 73cf… 3115033      3115033           Ryan      
#>  3 Centers        5291935 s:70~… a477… 5291935      5291935           Anton     
#>  4 Centers        5080227 s:70~… 0825… 5080227      5080227           Ryan      
#>  5 Centers        5149194 s:70~… 4852… 5149194      5149194           Oliver    
#>  6 Centers        5080157 s:70~… eaef… 5080157      5080157           Frank     
#>  7 Centers        2592095 s:70~… 527b… 2592095      2592095           Teuvo     
#>  8 Left Wings     3042056 s:70~… 5075… 3042056      3042056           Tyler     
#>  9 Left Wings     5090524 s:70~… bce2… 5090524      5090524           Dillon    
#> 10 Left Wings     3900260 s:70~… 16ee… 3900260      3900260           Jordan    
#> # ℹ 15 more rows
#> # ℹ 46 more variables: last_name <chr>, full_name <chr>, display_name <chr>,
#> #   short_name <chr>, weight <dbl>, display_weight <chr>, height <dbl>,
#> #   display_height <chr>, age <int>, date_of_birth <chr>, slug <chr>,
#> #   jersey <chr>, headshot_href <chr>, headshot_alt <chr>, hand_type <chr>,
#> #   hand_abbreviation <chr>, hand_display_value <chr>, position_id <chr>,
#> #   position_name <chr>, position_display_name <chr>, …
# }
```
