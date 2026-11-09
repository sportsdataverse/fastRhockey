# **Get ESPN MCH / WCH League Root (core-v2)**

**Get ESPN MCH / WCH League Root (core-v2)**

## Usage

``` r
espn_mch_league_root(...)

espn_wch_league_root(...)
```

## Arguments

- ...:

  Reserved for forward compatibility.

## Value

A one-row `fastRhockey_data` tibble with league metadata:

|                |           |                                             |
|----------------|-----------|---------------------------------------------|
| col_name       | type      | description                                 |
| id             | character | ESPN league identifier (e.g. "90" for MCH). |
| guid           | character | League global unique identifier.            |
| uid            | character | League uid string.                          |
| name           | character | Full league name.                           |
| display_name   | character | League display name.                        |
| abbreviation   | character | League abbreviation (e.g. "MCH").           |
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

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_league_root()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_league_root.md);
columns verified against a captured MCH body.

`espn_wch_league_root()` is the same wrapper for women's college hockey
(league slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_mch_league_root())
#> ── MENS-COLLEGE-HOCKEY League Root data from ESPN core-v2 ──────────────────────
#> ℹ Data updated: 2026-11-09 11:49:18 UTC
#> # A tibble: 1 × 16
#>   id    guid              uid   name  display_name abbreviation short_name slug 
#>   <chr> <chr>             <chr> <chr> <chr>        <chr>        <chr>      <chr>
#> 1 91    383aeb8b-cbb8-37… s:70… NCAA… NCAA Men's … NCAAH        NCAAH      mens…
#> # ℹ 8 more variables: is_tournament <lgl>, gender <chr>, season_ref <chr>,
#> #   seasons_ref <chr>, groups_ref <chr>, franchises_ref <chr>, teams_ref <chr>,
#> #   events_ref <chr>
# }
# \donttest{
  try(espn_wch_league_root())
#> ── WOMENS-COLLEGE-HOCKEY League Root data from ESPN core-v2 ────────────────────
#> ℹ Data updated: 2026-11-09 11:49:18 UTC
#> # A tibble: 1 × 16
#>   id    guid              uid   name  display_name abbreviation short_name slug 
#>   <chr> <chr>             <chr> <chr> <chr>        <chr>        <chr>      <chr>
#> 1 92    a5346237-9479-31… s:70… NCAA… NCAA Women'… CWHOC        CWHOC      wome…
#> # ℹ 8 more variables: is_tournament <lgl>, gender <chr>, season_ref <chr>,
#> #   seasons_ref <chr>, groups_ref <chr>, franchises_ref <chr>, teams_ref <chr>,
#> #   events_ref <chr>
# }
```
