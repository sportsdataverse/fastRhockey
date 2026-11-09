# **Get ESPN NHL Team Detail**

**Get ESPN NHL Team Detail**

## Usage

``` r
espn_nhl_team(team_id, ...)
```

## Arguments

- team_id:

  ESPN team identifier (character or numeric, e.g. `"4"` for Chicago
  Blackhawks).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per team:

|                    |           |                                         |
|--------------------|-----------|-----------------------------------------|
| col_name           | types     | description                             |
| team_id            | character | ESPN team identifier (echoed from arg). |
| id                 | character | ESPN team identifier.                   |
| uid                | character | ESPN team uid.                          |
| slug               | character | URL slug.                               |
| location           | character | Team city/location.                     |
| name               | character | Team mascot name.                       |
| nickname           | character | Team nickname.                          |
| abbreviation       | character | Team abbreviation.                      |
| display_name       | character | Full display name.                      |
| short_display_name | character | Short display name.                     |
| color              | character | Primary color hex.                      |
| alternate_color    | character | Alternate color hex.                    |
| is_active          | logical   | Whether the team is active.             |
| logo               | character | Primary logo URL.                       |
| logo_dark          | character | Dark logo URL.                          |
| stand_summary      | character | Standing summary string.                |
| franchise_ref      | character | Franchise resource ref URL.             |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_team(team_id = "4"))
#> ── NHL Team data from ESPN.com ──────────────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 19:35:26 UTC
#> # A tibble: 1 × 17
#>   team_id id    uid      slug  location name  nickname abbreviation display_name
#>   <chr>   <chr> <chr>    <chr> <chr>    <chr> <chr>    <chr>        <chr>       
#> 1 4       4     s:70~l:… chic… Chicago  Blac… Chicago  CHI          Chicago Bla…
#> # ℹ 8 more variables: short_display_name <chr>, color <chr>,
#> #   alternate_color <chr>, is_active <lgl>, logo <chr>, logo_dark <chr>,
#> #   stand_summary <chr>, franchise_ref <chr>
# }
```
