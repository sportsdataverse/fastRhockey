# Seasons a player has stats for

Wraps `GET https://www.shl.se/api/statistics-v2/athlete/seasonList` of
the SHL site API (www.shl.se). Unofficial use of a public source;
seasons are END years (2026 = 2025/26) and are mapped to the site's ssgt
uuids through `/statistics-v2/layout-info`.

## Usage

``` r
shl_athlete_seasons(
  athlete_uuid,
  master_site_instance_id = NULL,
  return_parsed = TRUE
)
```

## Arguments

- athlete_uuid:

  Required. Athlete uuid.

- master_site_instance_id:

  Site instance (shl1_shl). No effect in the drop-one probe.

- return_parsed:

  Parse the payload into a tibble (default `TRUE`); `FALSE` returns the
  raw decoded JSON as a list.

## Value

A `fastRhockey_data` tibble (the raw JSON list when
`return_parsed = FALSE`) with the following columns:

|  |  |  |
|----|----|----|
| col_name | type | description |
| value | character | Season START year (2025 = 2025/2026); the season= param of playerProfile_seasonGameLog. Numeric value sent as a string. |
| name | character | Season label (e.g. 2025/2026). |
| label | character | Display label (season list: 2025/2026). |

## Errors

`fastRhockey_no_data`: the host answered 404, an empty body (gameday
routes before 2023/24), a table with no rows, or the season and game
type are not in layout-info. `fastRhockey_asset_fetch`: any other
non-2xx answer or a dead connection after retries, a non-JSON or
error-envelope 200, a stats-info page that reaches `count` or falls
short of its `totalCount`, or a body for another game, team, athlete or
ssgt than the one asked for. `fastRhockey_bad_request`: a 400 / 422, a
season that is not a year, or an argument outside its choices.

## See also

[family_shl](https://fastRhockey.sportsdataverse.org/reference/family_shl.md)
lists every function in this family.

## Examples

``` r
# \donttest{
  try(shl_athlete_seasons(athlete_uuid = "qQ9-349c7WDv6", master_site_instance_id = "shl1_shl"))
#> ── SHL Athlete Seasons Information from www.shl.se ──────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 20:22:55 UTC
#> # A tibble: 15 × 3
#>    value name      label    
#>    <chr> <chr>     <chr>    
#>  1 2026  2026/2027 2026/2027
#>  2 2025  2025/2026 2025/2026
#>  3 2024  2024/2025 2024/2025
#>  4 2023  2023/2024 2023/2024
#>  5 2022  2022/2023 2022/2023
#>  6 2015  2015/2016 2015/2016
#>  7 2014  2014/2015 2014/2015
#>  8 2013  2013/2014 2013/2014
#>  9 2012  2012/2013 2012/2013
#> 10 2011  2011/2012 2011/2012
#> 11 2010  2010/2011 2010/2011
#> 12 2009  2009/2010 2009/2010
#> 13 2008  2008/2009 2008/2009
#> 14 2007  2007/2008 2007/2008
#> 15 2006  2006/2007 2006/2007
# }
```
