# **Load NHL group aliases from the SportsDataverse data repo**

Every name and id a source uses for an NHL conference or division, with
the seasons it is valid for – the crosswalk from NHL and ESPN ids and
names to SportsDataverse group ids. Published to the `nhl_groups`
release tag on the sportsdataverse-data releases.

## Usage

``` r
load_nhl_group_aliases(..., dbConnection = NULL, tablename = NULL)
```

## Arguments

- ...:

  Additional arguments passed to an underlying function that writes the
  data into a database.

- dbConnection:

  A `DBIConnection` object, as returned by
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)

- tablename:

  The name of the data table within the database

## Value

A data frame (`fastRhockey_data`) with the following columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| league | character | League key. |
| group_id | character | SportsDataverse group id, `{league}:{slug}`. |
| source | character | Source that uses the alias (e.g. `nhl`, `espn`). |
| source_id | character | The source's own id for the group, when it has one. |
| name_kind | character | Alias kind: `name`, `short_name`, `abbreviation`, `slug` or `code`. |
| value | character | The alias. |
| valid_from | integer | First season (end year) the alias is valid (inclusive); `NA` = unbounded. |
| valid_to | integer | Last season (end year) the alias is valid (inclusive); `NA` = unbounded. |

## Examples

``` r
# \donttest{
  try(load_nhl_group_aliases())
#> ── NHL group aliases from the SportsDataverse data repo ─── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-10-09 03:19:40 UTC
#> # A tibble: 95 × 8
#>    league group_id          source source_id name_kind value valid_from valid_to
#>    <chr>  <chr>             <chr>  <chr>     <chr>     <chr>      <int>    <int>
#>  1 nhl    nhl:adams-northe… nhl    NA        abbrevia… ADM         1975     1993
#>  2 nhl    nhl:adams-northe… nhl    NA        abbrevia… NE          1994     2013
#>  3 nhl    nhl:adams-northe… nhl    NA        name      Adam…       1975     1993
#>  4 nhl    nhl:adams-northe… nhl    NA        name      Nort…       1994     2013
#>  5 nhl    nhl:adams-northe… nhl    NA        short_na… Adams       1975     1993
#>  6 nhl    nhl:adams-northe… nhl    NA        short_na… Nort…       1994     2013
#>  7 nhl    nhl:american      nhl    NA        abbrevia… AMR         1927     1938
#>  8 nhl    nhl:american      nhl    NA        name      Amer…       1927     1938
#>  9 nhl    nhl:american      nhl    NA        short_na… Amer…       1927     1938
#> 10 nhl    nhl:atlantic      espn   32        abbrevia… ATL         2014       NA
#> # ℹ 85 more rows
# }
```
