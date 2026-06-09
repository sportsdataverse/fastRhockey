# **QMJHL Statistical Leaders**

QMJHL statistical leaders for a season from the HockeyTech feed.

## Usage

``` r
qmjhl_leaders(season = NULL, season_id = NULL)
```

## Arguments

- season:

  End-year season (e.g. 2025); optional (defaults to most-recent).

- season_id:

  Explicit HockeyTech season id; optional.

## Value

A `fastRhockey_data` data frame, one row per player entry.

## See also

Other QMJHL Functions:
[`most_recent_qmjhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_qmjhl_season.md),
[`qmjhl`](https://fastRhockey.sportsdataverse.org/reference/qmjhl.md),
[`qmjhl_game_corsi()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_game_corsi.md),
[`qmjhl_game_shifts()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_game_shifts.md),
[`qmjhl_game_summary()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_game_summary.md),
[`qmjhl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_pbp.md),
[`qmjhl_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_player_stats.md),
[`qmjhl_player_toi()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_player_toi.md),
[`qmjhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_schedule.md),
[`qmjhl_season_id()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_season_id.md),
[`qmjhl_standings()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_standings.md),
[`qmjhl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_team_roster.md),
[`qmjhl_teams()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_teams.md)

## Examples

``` r
 try(qmjhl_leaders()) 
#> ── QMJHL Leaders from HockeyTech ────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-06-09 20:30:42 UTC
#> # A tibble: 10 × 15
#>     rank player_id jersey_number name      team_id team_name team_code team_logo
#>    <int> <chr>     <chr>         <chr>     <chr>   <chr>     <chr>     <chr>    
#>  1     1 19517     7             Maxim Ma… 10      Chicouti… Chi       https://…
#>  2     2 20174     44            Philippe… 15      Val-d'Or… VdO       https://…
#>  3     3 19113     18            Thomas V… 11      Rouyn-No… Rou       https://…
#>  4     4 19110     9             Justin L… 2       Newfound… NFL       https://…
#>  5     5 19536     55            Félix La… 13      Shawinig… Sha       https://…
#>  6     1 19517     7             Maxim Ma… 10      Chicouti… Chi       https://…
#>  7     2 19534     8             Justin C… 19      Blainvil… BLB       https://…
#>  8     3 21909     24            Nathan L… 7       Charlott… Cha       https://…
#>  9     4 23770     17            Alexey V… 17      Victoria… Vic       https://…
#> 10     5 20174     44            Philippe… 15      Val-d'Or… VdO       https://…
#> # ℹ 7 more variables: team_logo_small <chr>, stat_formatted <chr>,
#> #   type_formatted <chr>, photo <chr>, photo_small <chr>, position <chr>,
#> #   division <chr>
```
