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
  try(elh_game_ids(season = 2027))
#> ── ELH Game Ids Information from hokej.cz ──────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 05:43:22 UTC
#> # A tibble: 2,812 × 5
#>    hokejcz_game_id season key                          last_modified        size
#>              <int>  <int> <chr>                        <chr>               <int>
#>  1         2928255   2027 match/2026/full/2928255.json 2026-09-16T18:57:2… 76217
#>  2         2928256   2027 match/2026/full/2928256.json 2026-09-16T18:24:4… 98469
#>  3         2928257   2027 match/2026/full/2928257.json 2026-09-16T20:46:3… 81005
#>  4         2928258   2027 match/2026/full/2928258.json 2026-09-16T18:18:0… 60920
#>  5         2928259   2027 match/2026/full/2928259.json 2026-09-16T19:19:5… 97082
#>  6         2928260   2027 match/2026/full/2928260.json 2026-09-15T18:40:4… 59501
#>  7         2928261   2027 match/2026/full/2928261.json 2026-09-16T18:14:5… 82570
#>  8         2928262   2027 match/2026/full/2928262.json 2026-09-04T19:13:5…   446
#>  9         2928263   2027 match/2026/full/2928263.json 2026-09-18T18:09:5… 65661
#> 10         2928264   2027 match/2026/full/2928264.json 2026-09-18T18:23:3… 54290
#> # ℹ 2,802 more rows
# }
```
