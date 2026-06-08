# **NHL Records - Award Details**

Returns detailed NHL award winners from the NHL Records API
(`https://records.nhl.com/site/api/award-details`). The endpoint accepts
a Cayenne filter on `seasonId` (other paths such as `franchiseId` and
`award-details/{id}` return errors).

## Usage

``` r
nhl_records_award_details(season_id = NULL, cayenne_exp = NULL)
```

## Arguments

- season_id:

  Optional integer 8-digit season ID (e.g., `20232024`). When supplied,
  becomes the cayenne filter `seasonId={season_id}`. Ignored when
  `cayenne_exp` is also supplied.

- cayenne_exp:

  Optional Cayenne filter expression string passed via the `cayenneExp`
  query parameter.

## Value

A data frame (`fastRhockey_data`) with the following columns:

|                      |           |                                            |
|----------------------|-----------|--------------------------------------------|
| col_name             | types     | description                                |
| id                   | integer   | Unique award detail identifier.            |
| awarded_posthumously | logical   | Whether the award was given posthumously.  |
| coach_id             | integer   | Coach identifier, if a coach award.        |
| created_on           | character | Timestamp the record was created.          |
| detail_summary       | logical   | Detail summary flag.                       |
| full_name            | character | Full name of the award winner.             |
| general_manager_id   | integer   | General manager identifier, if applicable. |
| image_url            | character | URL to the award image.                    |
| is_rookie            | logical   | Whether the winner was a rookie.           |
| player_id            | integer   | Unique player identifier.                  |
| player_image_caption | logical   | Player image caption flag.                 |
| player_image_url     | character | URL to the player image.                   |
| season_id            | integer   | 8-digit season identifier.                 |
| status               | character | Award status.                              |
| summary              | character | Award summary text.                        |
| team_id              | integer   | Unique team identifier.                    |
| trophy_category_id   | integer   | Trophy category identifier.                |
| trophy_id            | integer   | Trophy identifier.                         |
| value                | integer   | Award value or statistical measure.        |
| vote_count           | integer   | Number of votes received.                  |

## Examples

``` r
# \donttest{
  try(nhl_records_award_details())
#> ── NHL Records Award Details ────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-06-08 11:44:28 UTC
#> # A tibble: 5,393 × 20
#>       id awarded_posthumously coach_id created_on       detail_summary full_name
#>    <int> <lgl>                   <int> <chr>            <lgl>          <chr>    
#>  1     1 FALSE                      NA 2021-02-17T05:3… NA             Zach Bog…
#>  2     2 FALSE                      NA 2021-02-17T05:3… NA             Erik Cer…
#>  3     3 FALSE                      NA 2021-02-17T05:3… NA             Anthony …
#>  4     4 FALSE                      NA 2021-02-17T05:3… NA             Blake Co…
#>  5     5 FALSE                      NA 2021-02-17T05:3… NA             Braydon …
#>  6     6 FALSE                      NA 2021-02-17T05:3… NA             Barclay …
#>  7     7 FALSE                      NA 2021-02-17T05:3… NA             Yanni Go…
#>  8     8 FALSE                      NA 2021-02-17T05:3… NA             Victor H…
#>  9     9 FALSE                      NA 2021-02-17T05:3… NA             Tyler Jo…
#> 10    10 FALSE                      NA 2021-02-17T05:3… NA             Alex Kil…
#> # ℹ 5,383 more rows
#> # ℹ 14 more variables: general_manager_id <int>, image_url <chr>,
#> #   is_rookie <lgl>, player_id <int>, player_image_caption <lgl>,
#> #   player_image_url <chr>, season_id <int>, status <chr>, summary <chr>,
#> #   team_id <int>, trophy_category_id <int>, trophy_id <int>, value <int>,
#> #   vote_count <int>
  try(nhl_records_award_details(season_id = 20232024))
#> ── NHL Records Award Details ────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-06-08 11:44:28 UTC
#> # A tibble: 72 × 20
#>       id awarded_posthumously coach_id created_on       detail_summary full_name
#>    <int> <lgl>                   <int> <chr>            <lgl>          <chr>    
#>  1  5268 FALSE                      73 2024-06-24T23:1… NA             Florida …
#>  2  5269 FALSE                    1880 2024-06-24T23:1… NA             Edmonton…
#>  3  5286 FALSE                      NA 2024-07-11T09:3… NA             Aleksand…
#>  4  5287 FALSE                      NA 2024-07-11T09:3… NA             Sam Benn…
#>  5  5288 FALSE                      NA 2024-07-11T09:3… NA             Nick Cou…
#>  6  5289 FALSE                      NA 2024-07-11T09:3… NA             Aaron Ek…
#>  7  5290 FALSE                      NA 2024-07-11T09:4… NA             Oliver E…
#>  8  5291 FALSE                      NA 2024-07-11T09:4… NA             Gustav F…
#>  9  5292 FALSE                      NA 2024-07-11T09:4… NA             Jonah Ga…
#> 10  5293 FALSE                      NA 2024-07-11T09:4… NA             Dmitry K…
#> # ℹ 62 more rows
#> # ℹ 14 more variables: general_manager_id <int>, image_url <chr>,
#> #   is_rookie <lgl>, player_id <int>, player_image_caption <lgl>,
#> #   player_image_url <chr>, season_id <int>, status <chr>, summary <chr>,
#> #   team_id <int>, trophy_category_id <int>, trophy_id <int>, value <int>,
#> #   vote_count <int>
# }
```
