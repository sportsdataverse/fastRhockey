# **NHL Draft Prospects**

Returns current draft prospect rankings.

Uses the new NHL API endpoint at
`api-web.nhle.com/v1/draft/rankings/now`.

## Usage

``` r
nhl_draft_prospects()
```

## Value

A data frame (`fastRhockey_data`) with the following columns:

|                      |           |                                        |
|----------------------|-----------|----------------------------------------|
| col_name             | types     | description                            |
| last_name            | character | Prospect's last name.                  |
| first_name           | character | Prospect's first name.                 |
| position_code        | character | Prospect's position code.              |
| shoots_catches       | character | Prospect's shooting/catching hand.     |
| height_in_inches     | integer   | Prospect's height in inches.           |
| weight_in_pounds     | integer   | Prospect's weight in pounds.           |
| last_amateur_club    | character | Prospect's most recent amateur club.   |
| last_amateur_league  | character | Prospect's most recent amateur league. |
| birth_date           | character | Prospect's birth date.                 |
| birth_city           | character | Prospect's birth city.                 |
| birth_state_province | character | Prospect's birth state or province.    |
| birth_country        | character | Prospect's birth country.              |
| midterm_rank         | integer   | Prospect's midterm draft ranking.      |
| final_rank           | integer   | Prospect's final draft ranking.        |

## Examples

``` r
# \donttest{
   try(nhl_draft_prospects())
#> ── NHL Draft Prospects data from NHL.com ────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-06-12 14:19:43 UTC
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
