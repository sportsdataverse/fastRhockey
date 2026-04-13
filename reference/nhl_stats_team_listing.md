# **NHL Stats API — Team Listing**

Returns the top-level team listing from the NHL Stats REST API. When
`team_id` is `NULL` (default) this hits
`https://api.nhle.com/stats/rest/{lang}/team` and returns the full
league-wide listing; when `team_id` is supplied it hits
`https://api.nhle.com/stats/rest/{lang}/team/id/{team_id}` and returns a
single-team payload. Distinct from
[`nhl_stats_teams()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_teams.md),
which hits the per-report stats endpoints.

## Usage

``` r
nhl_stats_team_listing(team_id = NULL, lang = "en", limit = 100, start = 0)
```

## Arguments

- team_id:

  Optional integer team id. If supplied, the by-id endpoint is used and
  pagination params are ignored.

- lang:

  Character language code. Default `"en"`.

- limit:

  Integer maximum number of results. Default 100. Ignored when `team_id`
  is supplied.

- start:

  Integer pagination start index. Default 0. Ignored when `team_id` is
  supplied.

## Value

A `fastRhockey_data` tibble of teams, or `NULL` on failure.

## Examples

``` r
# \donttest{
  try(nhl_stats_team_listing())
#> ── NHL Stats Team Listing ───────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:06:04 UTC
#> # A tibble: 62 × 6
#>       id franchise_id full_name              league_id raw_tricode tri_code
#>    <int>        <int> <chr>                      <int> <chr>       <chr>   
#>  1    32           27 Quebec Nordiques             133 QUE         QUE     
#>  2     8            1 Montréal Canadiens           133 MTL         MTL     
#>  3    58            5 Toronto St. Patricks         133 TSP         TSP     
#>  4     7           19 Buffalo Sabres               133 BUF         BUF     
#>  5    46           13 Oakland Seals                133 OAK         OAK     
#>  6    48           23 Kansas City Scouts           133 KCS         KCS     
#>  7    36            3 Ottawa Senators (1917)       133 SEN         SEN     
#>  8    70           NA To be determined             133 TBD         TBD     
#>  9    11           35 Atlanta Thrashers            133 ATL         ATL     
#> 10    45            3 St. Louis Eagles             133 SLE         SLE     
#> # ℹ 52 more rows
  try(nhl_stats_team_listing(team_id = 10))
#> ── NHL Stats Team Listing ───────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:06:04 UTC
#> # A tibble: 1 × 6
#>      id franchise_id full_name           league_id raw_tricode tri_code
#>   <int>        <int> <chr>                   <int> <chr>       <chr>   
#> 1    10            5 Toronto Maple Leafs       133 TOR         TOR     
# }
```
