# **Get ESPN NHL Season Draft Round Picks (core-v2)**

**Get ESPN NHL Season Draft Round Picks (core-v2)**

## Usage

``` r
espn_nhl_season_draft_round_picks(
  season = most_recent_nhl_season(),
  round_num = 1,
  limit = 100,
  ...
)
```

## Arguments

- season:

  Season end-year (e.g. `2025`). Defaults to
  [`most_recent_nhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_nhl_season.md).

- round_num:

  Draft round number (default `1`). Used to build the path
  `seasons/{season}/draft/rounds/{round_num}/picks`.

- limit:

  Maximum picks to return per page (default `100`).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per draft pick, or an empty
tibble when the endpoint is unavailable. The confirmed path is
`seasons/{season}/draft/rounds/{round_num}/picks`; for NHL this returns
HTTP 404 - the draft data is not served by the core-v2 sub-tree:

|            |           |                                       |
|------------|-----------|---------------------------------------|
| col_name   | types     | description                           |
| ref        | character | `$ref` URL for the draft pick object. |
| pick_id    | character | Pick id parsed from the `$ref` URL.   |
| season     | integer   | Season year (echoed from arg).        |
| round_num  | integer   | Round number (echoed from arg).       |
| count      | integer   | Total picks in the collection.        |
| page_count | integer   | Total pages in the collection.        |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  # NOTE: Returns an empty tibble for NHL (404 from ESPN)
  try(espn_nhl_season_draft_round_picks(season = 2025, round_num = 1))
#> ! 2026-11-25 20:18:22.369704: ESPN nhl season draft round picks for 2025 round 1 is not available (HTTP 404 is normal for NHL).
#> ! Warning: HTTP 404 Not Found from https://sports.core.api.espn.com/v2/sports/hockey/leagues/nhl/seasons/2025/draft/rounds/1/picks
#> data frame with 0 columns and 0 rows
# }
```
