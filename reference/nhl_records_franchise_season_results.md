# **NHL Records - Franchise Season Results**

Returns franchise season-by-season results from the NHL Records API
(`https://records.nhl.com/site/api/franchise-season-results`).

## Usage

``` r
nhl_records_franchise_season_results(franchise_id = NULL, cayenne_exp = NULL)
```

## Arguments

- franchise_id:

  Optional integer franchise ID. When supplied it is translated into a
  `cayenneExp=franchiseId={franchise_id}` filter unless `cayenne_exp` is
  already provided.

- cayenne_exp:

  Optional Cayenne filter expression string. Takes precedence over
  `franchise_id` when both are supplied.

## Value

A data frame (`fastRhockey_data`) with the following columns:

|                      |           |                                          |
|----------------------|-----------|------------------------------------------|
| col_name             | types     | description                              |
| id                   | integer   | Unique season result record identifier.  |
| conference_abbrev    | character | Conference abbreviation.                 |
| conference_name      | character | Conference name.                         |
| conference_sequence  | integer   | Conference standings position.           |
| decision             | character | Series/season decision indicator.        |
| division_abbrev      | character | Division abbreviation.                   |
| division_name        | character | Division name.                           |
| division_sequence    | integer   | Division standings position.             |
| final_playoff_round  | integer   | Final playoff round reached.             |
| franchise_id         | integer   | Unique franchise identifier.             |
| game_type_id         | integer   | Game type identifier (regular/playoffs). |
| games_played         | integer   | Games played in the season.              |
| goals                | integer   | Goals scored.                            |
| goals_against        | integer   | Goals against.                           |
| home_losses          | integer   | Losses at home.                          |
| home_overtime_losses | integer   | Overtime losses at home.                 |
| home_ties            | integer   | Ties at home.                            |
| home_wins            | integer   | Wins at home.                            |
| in_playoffs          | logical   | Whether the season reached the playoffs. |
| league_sequence      | integer   | League standings position.               |
| losses               | integer   | Total losses.                            |
| overtime_losses      | integer   | Total overtime losses.                   |
| penalty_minutes      | integer   | Penalty minutes.                         |
| playoff_round        | integer   | Playoff round identifier.                |
| points               | integer   | Standings points earned.                 |
| road_losses          | integer   | Losses on the road.                      |
| road_overtime_losses | integer   | Overtime losses on the road.             |
| road_ties            | integer   | Ties on the road.                        |
| road_wins            | integer   | Wins on the road.                        |
| season_id            | integer   | Season identifier.                       |
| series_abbrev        | character | Playoff series abbreviation.             |
| series_title         | character | Playoff series title.                    |
| shutouts             | integer   | Shutouts recorded.                       |
| team_id              | integer   | Unique team identifier.                  |
| team_name            | character | Team name.                               |
| ties                 | integer   | Total ties.                              |
| tri_code             | character | Team three-letter code.                  |
| wins                 | integer   | Total wins.                              |

## Examples

``` r
# \donttest{
  try(nhl_records_franchise_season_results())
#> ── NHL Records Franchise Season Results ─────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-06-12 22:25:51 UTC
#> # A tibble: 2,833 × 38
#>       id conference_abbrev conference_name conference_sequence decision
#>    <int> <chr>             <chr>                         <int> <chr>   
#>  1  2476 NA                NA                                0 L       
#>  2  2477 NA                NA                                0 NA      
#>  3  2478 NA                NA                                0 NA      
#>  4  2479 NA                NA                                0 W       
#>  5  2480 NA                NA                                0 L       
#>  6  2481 NA                NA                                0 W       
#>  7  2482 NA                NA                                0 W       
#>  8  2483 NA                NA                                0 L       
#>  9  2484 NA                NA                                0 NA      
#> 10  2485 NA                NA                                0 W       
#> # ℹ 2,823 more rows
#> # ℹ 33 more variables: division_abbrev <chr>, division_name <chr>,
#> #   division_sequence <int>, final_playoff_round <int>, franchise_id <int>,
#> #   game_type_id <int>, games_played <int>, goals <int>, goals_against <int>,
#> #   home_losses <int>, home_overtime_losses <int>, home_ties <int>,
#> #   home_wins <int>, in_playoffs <lgl>, league_sequence <int>, losses <int>,
#> #   overtime_losses <int>, penalty_minutes <int>, playoff_round <int>, …
# }
```
