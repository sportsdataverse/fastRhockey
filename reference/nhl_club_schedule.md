# **NHL Club Schedule**

Returns schedule data for a given team. Supports season, month, and week
views.

## Usage

``` r
nhl_club_schedule(
  team_abbr,
  season = NULL,
  month = NULL,
  view = "season",
  date = NULL
)
```

## Arguments

- team_abbr:

  Three-letter team abbreviation (e.g., "TOR", "BOS")

- season:

  Integer 4-digit year (e.g., 2024 for the 2024-25 season). If NULL,
  returns current season schedule.

- month:

  Character month in "YYYY-MM" format (e.g., "2025-01"). If provided,
  returns that month's schedule. Ignored if view is "week".

- view:

  Character: "season" (default), "month", or "week".

- date:

  Character date in "YYYY-MM-DD" format for week view. If NULL with
  view="week", returns current week.

## Value

Returns a data frame with schedule data.

## Examples

``` r
# \donttest{
  try(nhl_club_schedule(team_abbr = "TOR"))
#> ── NHL Club Schedule ────────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-07 07:09:12 UTC
#> # A tibble: 88 × 73
#>            id   season game_type game_date  neutral_site start_time_utc      
#>         <int>    <int>     <int> <chr>      <lgl>        <chr>               
#>  1 2025010010 20252026         1 2025-09-21 FALSE        2025-09-21T19:00:00Z
#>  2 2025010025 20252026         1 2025-09-23 FALSE        2025-09-23T23:00:00Z
#>  3 2025010037 20252026         1 2025-09-25 FALSE        2025-09-25T23:00:00Z
#>  4 2025010053 20252026         1 2025-09-27 FALSE        2025-09-27T23:00:00Z
#>  5 2025010079 20252026         1 2025-10-02 FALSE        2025-10-02T23:00:00Z
#>  6 2025010100 20252026         1 2025-10-04 FALSE        2025-10-04T23:00:00Z
#>  7 2025020004 20252026         2 2025-10-08 FALSE        2025-10-08T23:00:00Z
#>  8 2025020025 20252026         2 2025-10-11 FALSE        2025-10-11T23:00:00Z
#>  9 2025020043 20252026         2 2025-10-13 FALSE        2025-10-13T18:00:00Z
#> 10 2025020049 20252026         2 2025-10-14 FALSE        2025-10-14T23:00:00Z
#> # ℹ 78 more rows
#> # ℹ 67 more variables: eastern_utc_offset <chr>, venue_utc_offset <chr>,
#> #   venue_timezone <chr>, game_state <chr>, game_schedule_state <chr>,
#> #   tv_broadcasts <list>, three_min_recap <chr>, game_center_link <chr>,
#> #   three_min_recap_fr <chr>, condensed_game_fr <chr>, condensed_game <chr>,
#> #   tickets_link <chr>, tickets_link_fr <chr>, venue_default <chr>,
#> #   venue_fr <chr>, venue_es <chr>, away_team_id <int>, …
# }
```
