# **Get ESPN NHL Player Core (core-v2)**

**Get ESPN NHL Player Core (core-v2)**

## Usage

``` r
espn_nhl_player_core(athlete_id, ...)
```

## Arguments

- athlete_id:

  ESPN athlete identifier (character or numeric).

- ...:

  Reserved for forward compatibility.

## Value

A one-row `fastRhockey_data` tibble with athlete metadata:

|                   |           |                                         |
|-------------------|-----------|-----------------------------------------|
| col_name          | types     | description                             |
| athlete_id        | character | ESPN athlete id (echoed from arg).      |
| uid               | character | Athlete uid string.                     |
| guid              | character | Athlete global unique identifier.       |
| type              | character | Sport type (e.g. "hockey").             |
| first_name        | character | First name.                             |
| last_name         | character | Last name.                              |
| full_name         | character | Full display name.                      |
| display_name      | character | Display name.                           |
| short_name        | character | Short name (e.g. "P. Laine").           |
| weight            | numeric   | Weight in pounds.                       |
| display_weight    | character | Formatted weight string.                |
| height            | numeric   | Height in inches.                       |
| display_height    | character | Formatted height string.                |
| age               | integer   | Current age.                            |
| date_of_birth     | character | Date of birth (ISO 8601).               |
| debut_year        | integer   | Year of NHL debut.                      |
| birth_city        | character | Birth city.                             |
| birth_country     | character | Birth country abbreviation.             |
| slug              | character | URL slug.                               |
| jersey            | character | Jersey number.                          |
| hand              | character | Handedness (e.g. "L" or "R").           |
| active            | logical   | Whether athlete is currently active.    |
| position_ref      | character | `$ref` URL for the position object.     |
| team_ref          | character | `$ref` URL for the current team object. |
| statistics_ref    | character | `$ref` URL for career statistics.       |
| contracts_ref     | character | `$ref` URL for contracts.               |
| statisticslog_ref | character | `$ref` URL for the statistics log.      |
| athlete_ref       | character | `$ref` URL for this athlete object.     |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_player_core(athlete_id = "4024820"))
#> ── NHL Player Core data from ESPN core-v2 ───────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 20:16:40 UTC
#> # A tibble: 1 × 28
#>   athlete_id uid         guid  type  first_name last_name full_name display_name
#>   <chr>      <chr>       <chr> <chr> <chr>      <chr>     <chr>     <chr>       
#> 1 4024820    s:70~l:90~… b065… hock… Patrik     Laine     Patrik L… Patrik Laine
#> # ℹ 20 more variables: short_name <chr>, weight <dbl>, display_weight <chr>,
#> #   height <dbl>, display_height <chr>, age <int>, date_of_birth <chr>,
#> #   debut_year <int>, birth_city <chr>, birth_country <chr>, slug <chr>,
#> #   jersey <chr>, hand <chr>, active <lgl>, position_ref <chr>, team_ref <chr>,
#> #   statistics_ref <chr>, contracts_ref <chr>, statisticslog_ref <chr>,
#> #   athlete_ref <chr>
# }
```
