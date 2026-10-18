# **Czech Extraliga (ELH) S3 bucket listing**

One page (up to 1,000 keys) of the hokej.cz S3 bucket listing
(ListObjects V1); match keys carry the hokej.cz game id. The bucket is
NOT Extraliga-only, and the listing has no league field: see
[`elh_game_ids()`](https://fastRhockey.sportsdataverse.org/reference/elh_game_ids.md).
Keys of a few hundred bytes are files written ahead of a game.

## Usage

``` r
elh_list_objects(
  prefix = NULL,
  delimiter = NULL,
  marker = NULL,
  max_keys = NULL,
  list_type = NULL,
  continuation_token = NULL,
  start_after = NULL,
  return_parsed = TRUE
)
```

## Arguments

- prefix:

  Key prefix, e.g. `match/2026/full/` or `visualization/shots/`.

- delimiter:

  `/` groups keys below the next slash into CommonPrefixes (folder
  view).

- marker:

  ListObjects V1 paging: the last `Key` of the previous page; repeat
  until `IsTruncated` is false (V1 sends `NextMarker` only with a
  delimiter).

- max_keys:

  Page size, at most 1000 (the default).

- list_type:

  `2` switches to ListObjectsV2 (`KeyCount`, `NextContinuationToken`);
  omitted = V1 as documented here.

- continuation_token:

  V2 only (`list_type = 2`): `NextContinuationToken` of the previous
  page.

- start_after:

  V2 only (`list_type = 2`): list keys after this key.

- return_parsed:

  Parse the page into a tibble (default `TRUE`). Pass `FALSE` for the
  raw ListObjects XML page as text.

## Value

A tibble (`return_parsed = TRUE`; `hokejcz_game_id` is `NA` for any key
that is not `match/{season}/full/{id}.json`), else the raw XML text:

|  |  |  |
|----|----|----|
| col_name | type | description |
| hokejcz_game_id | integer | hokej.cz game id (the S3 file name; shots match.id, scoreboard hokejcz_id). integer. |
| key | character | S3 object key, e.g. match/2026/full/2928255.json. |
| last_modified | character | Last write of the object, ISO-8601 UTC. |
| size | integer | Object size in bytes (a few hundred bytes = a file written ahead of the game). |

## Errors

`fastRhockey_no_data`: HTTP 404. `fastRhockey_asset_fetch`: a body that
is not a ListBucketResult, a truncated page without a key to page on, an
empty 200, or any other non-2xx. `fastRhockey_bad_request`: HTTP
400/422.

## See also

Other ELH Functions:
[`elh_events()`](https://fastRhockey.sportsdataverse.org/reference/elh_events.md),
[`elh_game_ids()`](https://fastRhockey.sportsdataverse.org/reference/elh_game_ids.md),
[`elh_pbp()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp.md),
[`elh_pbp_goals()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp_goals.md),
[`elh_pbp_latest()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp_latest.md),
[`elh_pbp_mobile()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp_mobile.md),
[`elh_scoreboard()`](https://fastRhockey.sportsdataverse.org/reference/elh_scoreboard.md),
[`elh_shots()`](https://fastRhockey.sportsdataverse.org/reference/elh_shots.md)

## Examples

``` r
# \donttest{
  try(elh_list_objects(prefix = "match/2026/full/", max_keys = "3"))
#> ── ELH List Objects Information from hokej.cz ──────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 05:43:22 UTC
#> # A tibble: 3 × 4
#>   hokejcz_game_id key                          last_modified             size
#>             <int> <chr>                        <chr>                    <int>
#> 1         2928255 match/2026/full/2928255.json 2026-09-16T18:57:27.000Z 76217
#> 2         2928256 match/2026/full/2928256.json 2026-09-16T18:24:43.000Z 98469
#> 3         2928257 match/2026/full/2928257.json 2026-09-16T20:46:31.000Z 81005
# }
```
