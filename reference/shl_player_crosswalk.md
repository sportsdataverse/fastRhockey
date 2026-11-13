# SHL player id crosswalk: isa id to athlete uuid

The SHL has three player id systems: the **isa** (StatNet) id that
play-by-play, box scores and stats tables key on, the platform **athlete
uuid** that rosters and athlete routes key on, and the old
`ramses_legacy` CMS id (athlete-details only). The `players_summary`
table's `players{}` map is keyed by isa and carries each player's uuid,
skaters and goalies alike, so one stats-info call per season gives the
whole crosswalk.

## Usage

``` r
shl_player_crosswalk(season, game_type = "regular")
```

## Arguments

- season:

  Season END year (2026 = 2025/26).

- game_type:

  Game type: `"regular"`, `"playoff"`, `"qualdown"` or `"playin"`.

## Value

A `fastRhockey_data` tibble, one row per player, with the following
columns (the API may add more, after these):

|  |  |  |
|----|----|----|
| col_name | type | description |
| player_id | integer | isa (StatNet) player id: the map key. The id play-by-play, box scores and stats tables key on. |
| athlete_uuid | character | Platform athlete uuid (qQ9-... or 10-char form); joins to the athlete routes and the roster. Null where SHL has none, common before about 2005. |
| first_name | character | Given (first) name of the player. |
| last_name | character | Surname (family name) of the player. |
| jersey_number | integer | Jersey (shirt) number of the player. |
| position | character | Position as the players map carries it (F, D, GK or a Swedish label). |
| shoots | character | Shooting side (L / R; Left / Right). |
| full_name | character | Given name and surname. |
| date_of_birth | character | Date of birth (YYYY-MM-DD). |
| nationality | character | Nationality (ISO country code). |
| gender | character | Sex of the athlete (male / female). |
| weight | integer | Weight of the player in kilograms. |
| height | integer | Height of the player in centimetres. |
| ep_url | character | External profile URL of the player (empty in the sample). |

## Errors

`fastRhockey_no_data`: layout-info lists no such ssgt, or the table has
no rows. `fastRhockey_asset_fetch`: the fetch failed or the table came
back cut.

## See also

[family_shl](https://fastRhockey.sportsdataverse.org/reference/family_shl.md)
lists every function in this family.

## Examples

``` r
# \donttest{
  try({
    xw <- shl_player_crosswalk(2026)
    xw[xw$player_id == 2427, ]
  })
#> ── SHL Player Crosswalk Information from www.shl.se ─────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 21:34:36 UTC
#> # A tibble: 1 × 14
#>   player_id athlete_uuid  first_name last_name jersey_number position shoots
#>       <int> <chr>         <chr>      <chr>             <int> <chr>    <chr> 
#> 1      2427 qQ9-349c7WDv6 Lars       Johansson             1 GK       L     
#> # ℹ 7 more variables: ep_url <chr>, full_name <chr>, date_of_birth <chr>,
#> #   nationality <chr>, gender <chr>, weight <int>, height <int>
# }
```
