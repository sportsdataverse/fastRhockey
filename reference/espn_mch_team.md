# **Get ESPN MCH / WCH Team Detail**

**Get ESPN MCH / WCH Team Detail**

## Usage

``` r
espn_mch_team(team_id, ...)

espn_wch_team(team_id, ...)
```

## Arguments

- team_id:

  ESPN team identifier (character or numeric, e.g. `"104"` for Chicago
  Blackhawks).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per team:

|                    |           |                                         |
|--------------------|-----------|-----------------------------------------|
| col_name           | type      | description                             |
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

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_team()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_team.md);
columns verified against a captured MCH body.

`espn_wch_team()` is the same wrapper for women's college hockey (league
slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_mch_team(team_id = "104"))
#> ── MENS-COLLEGE-HOCKEY Team data from ESPN.com ──────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 19:34:13 UTC
#> # A tibble: 1 × 17
#>   team_id id    uid      slug  location name  nickname abbreviation display_name
#>   <chr>   <chr> <chr>    <chr> <chr>    <chr> <chr>    <chr>        <chr>       
#> 1 104     104   s:70~l:… bost… Boston … Terr… Boston … BU           Boston Univ…
#> # ℹ 8 more variables: short_display_name <chr>, color <chr>,
#> #   alternate_color <chr>, is_active <lgl>, logo <chr>, logo_dark <chr>,
#> #   stand_summary <chr>, franchise_ref <chr>
# }
# \donttest{
  try(espn_wch_team(team_id = "2137"))
#> ── WOMENS-COLLEGE-HOCKEY Team data from ESPN.com ────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 19:34:13 UTC
#> # A tibble: 1 × 17
#>   team_id id    uid      slug  location name  nickname abbreviation display_name
#>   <chr>   <chr> <chr>    <chr> <chr>    <chr> <chr>    <chr>        <chr>       
#> 1 2137    2137  s:70~l:… clar… Clarkson Gold… Clarkson CLAR         Clarkson Go…
#> # ℹ 8 more variables: short_display_name <chr>, color <chr>,
#> #   alternate_color <chr>, is_active <lgl>, logo <chr>, logo_dark <chr>,
#> #   stand_summary <chr>, franchise_ref <chr>
# }
```
