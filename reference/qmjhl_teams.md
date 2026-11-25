# **QMJHL Teams**

QMJHL teams for a given season from the HockeyTech feed.

## Usage

``` r
qmjhl_teams(season = NULL, season_id = NULL)
```

## Arguments

- season:

  End-year season (e.g. 2025); optional (defaults to most-recent).

- season_id:

  Explicit HockeyTech season id; optional.

## Value

A `fastRhockey_data` data frame, one row per team.

## See also

Other QMJHL Functions:
[`most_recent_qmjhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_qmjhl_season.md),
[`qmjhl`](https://fastRhockey.sportsdataverse.org/reference/qmjhl.md),
[`qmjhl_game_corsi()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_game_corsi.md),
[`qmjhl_game_shifts()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_game_shifts.md),
[`qmjhl_game_summary()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_game_summary.md),
[`qmjhl_leaders()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_leaders.md),
[`qmjhl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_pbp.md),
[`qmjhl_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_player_stats.md),
[`qmjhl_player_toi()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_player_toi.md),
[`qmjhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_schedule.md),
[`qmjhl_season_id()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_season_id.md),
[`qmjhl_standings()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_standings.md),
[`qmjhl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_team_roster.md)

## Examples

``` r
 try(qmjhl_teams()) 
#> ── QMJHL Teams from HockeyTech ──────────────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-25 20:24:49 UTC
#> # A tibble: 22 × 7
#>    team_name       team_id team_code team_nickname team_label division team_logo
#>    <chr>           <chr>   <chr>     <chr>         <chr>      <chr>    <chr>    
#>  1 - Kingston, Fr… 71      Kin       Frontenacs    Kingston … 20       https://…
#>  2 - North Bay, B… 54      NtB       Batallion     North Bay… 20       https://…
#>  3 - Ottawa, 67's… 52      Ott       67's          Ottawa - … 20       https://…
#>  4 - Sudbury, Wol… 65      Sud       Wolves        Sudbury -… 20       https://…
#>  5 Baie-Comeau, D… 16      BaC       Drakkar       Baie-Come… 19       https://…
#>  6 Blainville-Boi… 19      BLB       Armada        Blainvill… 20       https://…
#>  7 Cape Breton, E… 3       Cap       Eagles        Cape Bret… 19       https://…
#>  8 Charlottetown,… 7       Cha       Islanders     Charlotte… 19       https://…
#>  9 Chicoutimi, Sa… 10      Chi       Saguenéens    Chicoutimi 19       https://…
#> 10 Drummondville,… 14      Dru       Voltigeurs    Drummondv… 20       https://…
#> # ℹ 12 more rows
```
