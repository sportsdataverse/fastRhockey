# **Get Fox Sports NHL play-by-play**

**Get Fox Sports NHL play-by-play**

## Usage

``` r
fox_nhl_pbp(game_id)
```

## Arguments

- game_id:

  Fox Bifrost event id (e.g. `"44398"`).

## Value

A `fastRhockey_data` tibble, one row per play: `game_id`, `period`,
`left_team`, `right_team`, `play_id`, `clock`, `team`,
`left_score_change`, `right_score_change`, `play_text`.

## Examples

``` r
 try(fox_nhl_pbp("44398")) 
#> ── Fox Sports NHL pbp ───────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-06-24 02:03:51 UTC
#> # A tibble: 817 × 10
#>    game_id period     left_team right_team play_id clock team  left_score_change
#>    <chr>   <chr>      <chr>     <chr>      <chr>   <chr> <chr> <chr>            
#>  1 44398   1ST PERIOD BOS       BUF        1       20:00 BOST… FALSE            
#>  2 44398   1ST PERIOD BOS       BUF        2       20:00 BUFF… FALSE            
#>  3 44398   1ST PERIOD BOS       BUF        3       20:00 BUFF… FALSE            
#>  4 44398   1ST PERIOD BOS       BUF        4       20:00 BOST… FALSE            
#>  5 44398   1ST PERIOD BOS       BUF        5       20:00 BUFF… FALSE            
#>  6 44398   1ST PERIOD BOS       BUF        6       20:00 BOST… FALSE            
#>  7 44398   1ST PERIOD BOS       BUF        8       19:41 BOST… FALSE            
#>  8 44398   1ST PERIOD BOS       BUF        9       19:41 BOST… FALSE            
#>  9 44398   1ST PERIOD BOS       BUF        10      19:41 BOST… FALSE            
#> 10 44398   1ST PERIOD BOS       BUF        11      19:41 BUFF… FALSE            
#> # ℹ 807 more rows
#> # ℹ 2 more variables: right_score_change <chr>, play_text <chr>
```
