# **Czech Extraliga (ELH) game ids of a season**

Every hokej.cz game id with a play-by-play file in a season, paging the
S3 listing. Lists `match/{season - 1}/full/` (the bucket files a season
under its START year) a thousand keys at a time, following `marker`
until the listing is complete.

## Usage

``` r
elh_game_ids(season)
```

## Arguments

- season:

  Season by its END year (2027 = 2026-27). Play-by-play starts in
  2014-15 (season 2015).

## Value

A tibble, one row per file, ordered by game id:

|  |  |  |
|----|----|----|
| col_name | type | description |
| hokejcz_game_id | integer | hokej.cz game id (the S3 file name; shots match.id, scoreboard hokejcz_id). integer. |
| season | integer | Season END year as requested (2027 = 2026-27). |
| key | character | S3 object key, e.g. match/2026/full/2928255.json. |
| last_modified | character | Last write of the object, ISO-8601 UTC. |
| size | integer | Object size in bytes (a few hundred bytes = a file written ahead of the game). |

## Details

**The bucket is NOT Extraliga-only.** A season folder holds every
competition hokej.cz shows (Maxa liga, lower leagues, juniors, the
national team): about 2,000 to 6,000 ids against about 400 ELH games,
plus files written ahead of a game (a few hundred bytes; see `size`).
The listing carries no league, so these ids are NOT filtered: check a
game's teams with
[`elh_shots()`](https://fastRhockey.sportsdataverse.org/reference/elh_shots.md)
/
[`elh_pbp()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp.md),
or, for a day still in the scoreboard's rolling window, keep the ids
[`elh_scoreboard()`](https://fastRhockey.sportsdataverse.org/reference/elh_scoreboard.md)
lists.

## Errors

`fastRhockey_no_data`: the season folder holds no play-by-play file.
`fastRhockey_asset_fetch`: a page failed, is not a listing, or the
listing does not advance. `fastRhockey_bad_request`: `season` is not a
year.

## See also

Other ELH Functions:
[`elh_events()`](https://fastRhockey.sportsdataverse.org/reference/elh_events.md),
[`elh_list_objects()`](https://fastRhockey.sportsdataverse.org/reference/elh_list_objects.md),
[`elh_pbp()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp.md),
[`elh_pbp_goals()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp_goals.md),
[`elh_pbp_latest()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp_latest.md),
[`elh_pbp_mobile()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp_mobile.md),
[`elh_scoreboard()`](https://fastRhockey.sportsdataverse.org/reference/elh_scoreboard.md),
[`elh_shots()`](https://fastRhockey.sportsdataverse.org/reference/elh_shots.md)

## Examples

``` r
# \donttest{
  # an early season: fewer listing pages (1,000 ids each) than a recent one
  try(elh_game_ids(season = 2016))
#> ── ELH Game Ids Information from hokej.cz ───────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 21:25:53 UTC
#> # A tibble: 1,972 × 5
#>    hokejcz_game_id season key                          last_modified        size
#>              <int>  <int> <chr>                        <chr>               <int>
#>  1          171741   2016 match/2015/full/171741.json  2016-03-21T16:12:…   8795
#>  2          218880   2016 match/2015/full/218880.json  2016-01-29T09:30:…  12756
#>  3         1693254   2016 match/2015/full/1693254.json 2015-07-14T11:58:…  76783
#>  4         2188519   2016 match/2015/full/2188519.json 2015-09-11T18:57:… 100494
#>  5         2188520   2016 match/2015/full/2188520.json 2015-09-11T18:39:…  71825
#>  6         2188521   2016 match/2015/full/2188521.json 2015-09-11T22:14:…  93246
#>  7         2188522   2016 match/2015/full/2188522.json 2015-09-11T18:49:…  43554
#>  8         2188523   2016 match/2015/full/2188523.json 2015-09-12T06:18:…  75784
#>  9         2188524   2016 match/2015/full/2188524.json 2015-09-24T10:36:…  46829
#> 10         2188525   2016 match/2015/full/2188525.json 2015-09-16T15:04:… 108006
#> # ℹ 1,962 more rows
# }
```
