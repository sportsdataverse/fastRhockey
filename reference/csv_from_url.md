# **Load .csv / .csv.gz file from a remote connection**

This is a thin wrapper on data.table::fread

## Usage

``` r
csv_from_url(...)
```

## Arguments

- ...:

  passed to data.table::fread

## Value

a dataframe as created by
[`data.table::fread()`](https://rdrr.io/pkg/data.table/man/fread.html)
