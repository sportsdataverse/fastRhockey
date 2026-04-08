# **Update or create a fastRhockey PWHL play-by-play database**

`update_pwhl_db()` updates or creates a database with `fastRhockey`
play-by-play data of all completed and available PWHL games since the
2024 inaugural season.

## Usage

``` r
update_pwhl_db(
  dbdir = ".",
  dbname = "fastRhockey_db",
  tblname = "fastRhockey_pwhl_pbp",
  force_rebuild = FALSE,
  db_connection = NULL
)
```

## Arguments

- dbdir:

  Directory in which the database is or shall be located

- dbname:

  File name of an existing or desired SQLite database within `dbdir`

- tblname:

  The name of the play-by-play data table within the database

- force_rebuild:

  Hybrid parameter (logical or numeric) to rebuild parts of or the
  complete play-by-play data table within the database (please see
  details for further information)

- db_connection:

  A `DBIConnection` object, as returned by
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)
  (please see details for further information)

## Value

Invisible NULL. Side effect: updates the database.

## Details

This function creates and updates a data table with the name `tblname`
within a SQLite database (other drivers via `db_connection`) located in
`dbdir` and named `dbname`. The data table combines all play-by-play
data for every available game back to the 2024 season and adds the most
recent completed games as soon as they are available for `fastRhockey`.

The argument `force_rebuild` is of hybrid type. It can rebuild the play-
by-play data table either for the whole fastRhockey PWHL era (with
`force_rebuild = TRUE`) or just for specified seasons (e.g.
`force_rebuild = 2024`). Please note the following behavior:

- `force_rebuild = TRUE`: The data table with the name `tblname` will be
  removed completely and rebuilt from scratch.

- `force_rebuild = c(2024, 2025)`: The data table with the name
  `tblname` will be preserved and only rows from the specified seasons
  will be deleted and re-added.

The parameter `db_connection` is intended for advanced users who want to
use other DBI drivers, such as MariaDB, Postgres or odbc. Please note
that the arguments `dbdir` and `dbname` are dropped in case a
`db_connection` is provided but the argument `tblname` will still be
used to write the data table into the database.

## Examples

``` r
if (FALSE) { # \dontrun{
  update_pwhl_db()
} # }
```
