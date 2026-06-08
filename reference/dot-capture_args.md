# Capture the calling function's bound formals (excluding `...`).

Capture the calling function's bound formals (excluding `...`).

## Usage

``` r
.capture_args()
```

## Value

Named list (empty if the caller has no non-... formals).

## Details

Must be called **directly** from the function whose arguments you want
to capture. It reads `sys.function(sys.parent())` and
[`parent.frame()`](https://rdrr.io/r/base/sys.parent.html) exactly one
level up; wrapping it inside another helper would return the wrong call
frame.
