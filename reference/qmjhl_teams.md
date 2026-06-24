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
#> ── QMJHL Teams from HockeyTech ──────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-06-24 02:07:17 UTC
#> # A tibble: 18 × 7
#>    team_name       team_id team_code team_nickname team_label division team_logo
#>    <chr>           <chr>   <chr>     <chr>         <chr>      <chr>    <chr>    
#>  1 Baie-Comeau, D… 16      BaC       Drakkar       Baie-Come… 19       https://…
#>  2 Blainville-Boi… 19      BLB       Armada        Blainvill… 20       https://…
#>  3 Cape Breton, E… 3       Cap       Eagles        Cape Bret… 19       https://…
#>  4 Charlottetown,… 7       Cha       Islanders     Charlotte… 19       https://…
#>  5 Chicoutimi, Sa… 10      Chi       Saguenéens    Chicoutimi 19       https://…
#>  6 Drummondville,… 14      Dru       Voltigeurs    Drummondv… 20       https://…
#>  7 Gatineau, Olym… 12      Gat       Olympiques    Gatineau   20       https://…
#>  8 Halifax, Moose… 5       Hal       Mooseheads    Halifax    19       https://…
#>  9 Moncton, Wildc… 1       Mon       Wildcats      Moncton    19       https://…
#> 10 Newfoundland, … 2       NFL       Regiment      Newfoundl… 19       https://…
#> 11 Québec, Rempar… 9       Que       Remparts      Québec     19       https://…
#> 12 Rimouski, Océa… 18      Rim       Océanic       Rimouski   19       https://…
#> 13 Rouyn-Noranda,… 11      Rou       Huskies       Rouyn-Nor… 20       https://…
#> 14 Saint John, Se… 8       SNB       Sea Dogs      Saint John 19       https://…
#> 15 Shawinigan, Ca… 13      Sha       Cataractes    Shawinigan 20       https://…
#> 16 Sherbrooke, Ph… 60      She       Phœnix        Sherbrooke 20       https://…
#> 17 Val-d'Or, Fore… 15      VdO       Foreurs       Val-d'Or   20       https://…
#> 18 Victoriaville,… 17      Vic       Tigres        Victoriav… 20       https://…
```
