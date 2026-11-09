# **Get ESPN NHL League Root (core-v2)**

**Get ESPN NHL League Root (core-v2)**

## Usage

``` r
espn_nhl_league_root(...)
```

## Arguments

- ...:

  Reserved for forward compatibility.

## Value

A one-row `fastRhockey_data` tibble with league metadata:

|                |           |                                             |
|----------------|-----------|---------------------------------------------|
| col_name       | types     | description                                 |
| id             | character | ESPN league identifier (e.g. "90" for NHL). |
| guid           | character | League global unique identifier.            |
| uid            | character | League uid string.                          |
| name           | character | Full league name.                           |
| display_name   | character | League display name.                        |
| abbreviation   | character | League abbreviation (e.g. "NHL").           |
| short_name     | character | League short name.                          |
| slug           | character | League slug (e.g. "nhl").                   |
| is_tournament  | logical   | Whether the league is a tournament.         |
| gender         | character | League gender designation.                  |
| season_ref     | character | `$ref` URL for the current season object.   |
| seasons_ref    | character | `$ref` URL for the seasons collection.      |
| groups_ref     | character | `$ref` URL for the groups collection.       |
| franchises_ref | character | `$ref` URL for the franchises collection.   |
| teams_ref      | character | `$ref` URL for the teams collection.        |
| events_ref     | character | `$ref` URL for the events collection.       |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_league_root())
#> ── NHL League Root data from ESPN core-v2 ───────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 19:34:38 UTC
#> # A tibble: 1 × 16
#>   id    guid              uid   name  display_name abbreviation short_name slug 
#>   <chr> <chr>             <chr> <chr> <chr>        <chr>        <chr>      <chr>
#> 1 90    1a5f0227-a13e-39… s:70… Nati… NHL          NHL          NHL        nhl  
#> # ℹ 8 more variables: is_tournament <lgl>, gender <chr>, season_ref <chr>,
#> #   seasons_ref <chr>, groups_ref <chr>, franchises_ref <chr>, teams_ref <chr>,
#> #   events_ref <chr>
# }
```
