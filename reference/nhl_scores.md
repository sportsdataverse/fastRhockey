# **NHL Scores**

Returns scores for all games on a given date.

## Usage

``` r
nhl_scores(date = NULL)
```

## Arguments

- date:

  Character date in "YYYY-MM-DD" format. If NULL, returns current
  scores.

## Value

Returns a data frame with game scores.

## Examples

``` r
# \donttest{
  try(nhl_scores())
#> 
#> NULL
# }
```
