# **Load fastRhockey PHF schedules**

helper that loads multiple seasons from the data repo either into memory
or writes it into a db using some forwarded arguments in the dots

**\[deprecated\]**

The PHF has ceased operations. This function is deprecated and will be
removed in a future release.

## Usage

``` r
load_phf_schedule(
  seasons = most_recent_phf_season(),
  ...,
  dbConnection = NULL,
  tablename = NULL
)
```

## Arguments

- seasons:

  A vector of 4-digit years associated with given PHF seasons. (Min:
  2011)

- ...:

  Additional arguments passed to an underlying function that writes the
  season data into a database (used by
  [`update_phf_db()`](https://fastRhockey.sportsdataverse.org/reference/update_phf_db.md)).

- dbConnection:

  A `DBIConnection` object, as returned by
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)

- tablename:

  The name of the schedule data table within the database

## Value

Returns a tibble

## Examples

``` r
# \donttest{
  try(load_phf_schedule(2021))
#> Error : `load_phf_schedule()` was deprecated in fastRhockey 1.0.0 and is now
#> defunct.
#> ℹ The PHF has ceased operations.
# }
```
