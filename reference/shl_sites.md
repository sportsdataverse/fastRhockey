# Teams (sites) taking part in an ssgt

Wraps `GET https://www.shl.se/api/sports-v2/all-sites/{ssgt_uuid}` of
the SHL site API (www.shl.se). Unofficial use of a public source;
seasons are END years (2026 = 2025/26) and are mapped to the site's ssgt
uuids through `/statistics-v2/layout-info`.

## Usage

``` r
shl_sites(ssgt_uuid, return_parsed = TRUE)
```

## Arguments

- ssgt_uuid:

  ssgt uuid: one season + series + game type (e.g. 2025/2026 SHL regular
  season). Directory: /statistics-v2/layout-info
  originalSeasonSeriesGameTypeTeams\[\].ssgtUuid.

- return_parsed:

  Parse the payload into a tibble (default `TRUE`); `FALSE` returns the
  raw decoded JSON as a list.

## Value

A `fastRhockey_data` tibble (the raw JSON list when
`return_parsed = FALSE`) with the following columns:

|  |  |  |
|----|----|----|
| col_name | type | description |
| id | character | Club site instance id (e.g. bif1_bif); equals instanceId. Not a team uuid or code. |
| instance_id | character | Club site instance id (e.g. fhc1_fhc). |
| public_url | character | Public image URL. |
| name | character | Full name of the club. |
| icon | character | URL of the team logo image. |

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
  try(shl_sites(ssgt_uuid = "iuzqg7dqk9"))
#> ── SHL Sites Information from www.shl.se ────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 20:23:12 UTC
#> # A tibble: 14 × 5
#>    id         instance_id public_url                     name           icon    
#>    <chr>      <chr>       <chr>                          <chr>          <chr>   
#>  1 bif1_bif   bif1_bif    http://www.brynas.se/          Brynäs IF      https:/…
#>  2 dif1_dif   dif1_dif    http://www.difhockey.se/       Djurgårdens IF https:/…
#>  3 fbk1_fbk   fbk1_fbk    http://www.farjestadbk.se/     Färjestad BK   https:/…
#>  4 fhc1_fhc   fhc1_fhc    http://www.frolundahockey.com/ Frölunda HC    https:/…
#>  5 hv711_hv71 hv711_hv71  http://www.hv71.se/            HV71           https:/…
#>  6 lhc1_lhc   lhc1_lhc    http://www.lhc.eu/             Linköping HC   https:/…
#>  7 lhf1_lhf   lhf1_lhf    http://www.luleahockey.se/     Luleå Hockey   https:/…
#>  8 lif1_lif   lif1_lif    http://www.leksandsif.se/      Leksands IF    https:/…
#>  9 mif1_mif   mif1_mif    http://www.malmoredhawks.com/  Malmö Redhawks https:/…
#> 10 ohk1_ohk   ohk1_ohk    http://www.orebrohockey.se/    Örebro Hockey  https:/…
#> 11 rbk1_rbk   rbk1_rbk    http://www.roglebk.se/         Rögle BK       https:/…
#> 12 saik1_saik saik1_saik  http://www.skellefteaaik.se/   Skellefteå AIK https:/…
#> 13 tik1_tik   tik1_tik    http://www.timraik.se/         Timrå IK       https:/…
#> 14 vlh1_vlh   vlh1_vlh    http://www.vaxjolakers.se/     Växjö Lakers   https:/…
# }
```
