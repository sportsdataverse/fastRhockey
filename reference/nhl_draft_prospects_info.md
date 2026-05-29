# **NHL Draft Prospects Info**

Returns draft prospect rankings for a given year and prospect category.

Uses the new NHL API endpoint at
`api-web.nhle.com/v1/draft/rankings/{year}/{prospect_category}`.

The original per-prospect-ID endpoint is no longer available. This
function now returns rankings filtered by year and category.

## Usage

``` r
nhl_draft_prospects_info(year, prospect_category = 1)
```

## Arguments

- year:

  Integer. Draft year (e.g. 2024).

- prospect_category:

  Integer. Prospect category:

  - 1 = North American Skater

  - 2 = International Skater

  - 3 = North American Goalie

  - 4 = International Goalie

## Value

A data frame (`fastRhockey_data`) with the following columns:

|                      |           |                                   |
|----------------------|-----------|-----------------------------------|
| col_name             | types     | description                       |
| last_name            | character | Prospect last name.               |
| first_name           | character | Prospect first name.              |
| position_code        | character | Player position code.             |
| shoots_catches       | character | Handedness (shoots/catches).      |
| height_in_inches     | integer   | Height in inches.                 |
| weight_in_pounds     | integer   | Weight in pounds.                 |
| last_amateur_club    | character | Last amateur club played for.     |
| last_amateur_league  | character | Last amateur league played in.    |
| birth_date           | character | Prospect birth date.              |
| birth_city           | character | Prospect birth city.              |
| birth_state_province | character | Prospect birth state or province. |
| birth_country        | character | Prospect birth country.           |
| midterm_rank         | integer   | Midterm ranking for the prospect. |
| final_rank           | integer   | Final ranking for the prospect.   |

## Examples

``` r
# \donttest{
   try(nhl_draft_prospects_info(year = 2024, prospect_category = 1))
#> ── NHL Draft Prospects Information from NHL.com ─────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-05-29 17:05:57 UTC
#> # A tibble: 251 × 14
#>    last_name first_name position_code shoots_catches height_in_inches
#>    <chr>     <chr>      <chr>         <chr>                     <int>
#>  1 Celebrini Macklin    C             L                            72
#>  2 Levshunov Artyom     D             R                            74
#>  3 Lindstrom Cayden     C             L                            75
#>  4 Buium     Zeev       D             L                            72
#>  5 Parekh    Zayne      D             R                            72
#>  6 Connelly  Trevor     LW            L                            73
#>  7 Dickinson Sam        D             L                            75
#>  8 Catton    Berkly     C             L                            70
#>  9 Iginla    Tij        C             L                            72
#> 10 Hage      Michael    C             R                            73
#> # ℹ 241 more rows
#> # ℹ 9 more variables: weight_in_pounds <int>, last_amateur_club <chr>,
#> #   last_amateur_league <chr>, birth_date <chr>, birth_city <chr>,
#> #   birth_state_province <chr>, birth_country <chr>, midterm_rank <int>,
#> #   final_rank <int>
# }
```
