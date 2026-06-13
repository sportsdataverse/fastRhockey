# **NHL Draft Rankings**

Returns NHL draft rankings for a given season and category.

## Usage

``` r
nhl_draft_rankings(season = NULL, category = 1)
```

## Arguments

- season:

  Integer 4-digit year (e.g., 2024). If NULL, returns current.

- category:

  Integer category: 1 = North American skaters, 2 = International
  skaters, 3 = North American goalies, 4 = International goalies.
  Default 1.

## Value

A data frame (`fastRhockey_data`) with the following columns:

|                      |           |                              |
|----------------------|-----------|------------------------------|
| col_name             | types     | description                  |
| last_name            | character | Prospect last name.          |
| first_name           | character | Prospect first name.         |
| position_code        | character | Player position code.        |
| shoots_catches       | character | Handedness (shoots/catches). |
| height_in_inches     | integer   | Height in inches.            |
| weight_in_pounds     | integer   | Weight in pounds.            |
| last_amateur_club    | character | Most recent amateur club.    |
| last_amateur_league  | character | Most recent amateur league.  |
| birth_date           | character | Date of birth.               |
| birth_city           | character | City of birth.               |
| birth_state_province | character | State or province of birth.  |
| birth_country        | character | Country of birth.            |
| midterm_rank         | integer   | Midterm draft ranking.       |
| final_rank           | integer   | Final draft ranking.         |

## Examples

``` r
# \donttest{
  try(nhl_draft_rankings())
#> ── NHL Draft Rankings ───────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-06-13 02:49:09 UTC
#> # A tibble: 253 × 14
#>    last_name first_name position_code shoots_catches height_in_inches
#>    <chr>     <chr>      <chr>         <chr>                     <int>
#>  1 McKenna   Gavin      LW            L                            71
#>  2 Reid      Chase      D             R                            74
#>  3 Carels    Carson     D             L                            74
#>  4 Verhoeff  Keaton     D             R                            76
#>  5 Rudolph   Daxon      D             R                            74
#>  6 Malhotra  Caleb      C             L                            74
#>  7 Lawrence  Tynan      C             L                            72
#>  8 Klepov    Nikita     RW            L                            72
#>  9 Belchetz  Ethan      LW            L                            77
#> 10 Morozov   Ilya       C             L                            75
#> # ℹ 243 more rows
#> # ℹ 9 more variables: weight_in_pounds <int>, last_amateur_club <chr>,
#> #   last_amateur_league <chr>, birth_date <chr>, birth_city <chr>,
#> #   birth_state_province <chr>, birth_country <chr>, midterm_rank <int>,
#> #   final_rank <int>
# }
```
