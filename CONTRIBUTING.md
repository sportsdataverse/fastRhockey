# Contributing to fastRhockey

Thank you for your interest in contributing to fastRhockey! This
document provides guidelines for contributing to the project.

## Getting Started

### Prerequisites

- R \>= 4.0.0
- [devtools](https://devtools.r-lib.org/) package
- Git

### Development Setup

``` r
# Clone the repository
# git clone https://github.com/sportsdataverse/fastRhockey.git

# Install all dependencies (including Suggests)
devtools::install_deps(dependencies = TRUE)

# Build documentation
devtools::document()

# Verify everything works
devtools::check()
```

## Development Workflow

1.  **Create a branch** from `main` using the naming convention:
    `<type>/<short-description>` (e.g., `feat/add-pwhl-draft`,
    `fix/schedule-null-handling`)

2.  **Make your changes** following the coding conventions below

3.  **Write tests** for any new or changed functions

4.  **Run checks** before submitting:

    ``` r
    devtools::document()  # Regenerate docs
    devtools::test()      # Run tests
    devtools::check()     # Full R CMD check
    ```

5.  **Submit a pull request** using the PR template

## Coding Conventions

### File Organization

- One exported function per file
- Filename matches the function name (e.g., `nhl_standings.R` contains
  [`nhl_standings()`](https://fastRhockey.sportsdataverse.org/reference/nhl_standings.md))
- Internal helper functions prefixed with `.` (e.g.,
  `.parse_response()`)

### Style Guide

- Use `%>%` (magrittr pipe) for chaining
- Use
  [`janitor::clean_names()`](https://sfirke.github.io/janitor/reference/clean_names.html)
  on all API responses
- Use `httr::RETRY("GET", ...)` for HTTP requests (built-in retry)
- Use [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html)
  for URL string interpolation
- Maximum line length: no strict limit, but keep readable

### Error Handling

All API-calling functions must follow this pattern:

``` r
tryCatch(
    expr = {
        res <- httr::RETRY("GET", url)
        check_status(res)
        # ... parse and return
    },
    error = function(e) {
        message(glue::glue("{Sys.time()}: Error in func_name: {e$message}"))
        return(NULL)
    }
)
```

Functions return `NULL` on failure – never throw errors to the user.

### S3 Class

All data-returning functions must wrap output with
`make_fastRhockey_data()`:

``` r
result <- make_fastRhockey_data(df, type = "Description of data", timestamp = Sys.time())
```

### Documentation

- All exported functions require roxygen2 documentation
- Include `@examples` with `\donttest{ try(...) }` wrapper
- Run `devtools::document()` to regenerate `man/` pages

## General Naming Conventions

### Exported Functions

Use the `league_entity_action()` pattern:

- [`nhl_teams_roster()`](https://fastRhockey.sportsdataverse.org/reference/nhl_teams_roster.md)
  – NHL team roster
- [`pwhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_schedule.md)
  – PWHL schedule
- [`phf_standings()`](https://fastRhockey.sportsdataverse.org/reference/phf_standings.md)
  – PHF standings
- [`nhl_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/nhl_player_stats.md)
  – NHL player statistics

### Internal Helpers

Prefix with a leading dot, use `snake_case`:

- [`.parse_game_rosters()`](https://fastRhockey.sportsdataverse.org/reference/dot-parse_game_rosters.md)
- [`.build_pbp()`](https://fastRhockey.sportsdataverse.org/reference/dot-build_pbp.md)
- [`.extract_scores()`](https://fastRhockey.sportsdataverse.org/reference/dot-extract_scores.md)

### Test Files

Name test files `test-function_name.R` to match the function under test:

- `test-nhl_standings.R`
- `test-pwhl_schedule.R`

### Datasets

Use `snake_case` for `.rda` files in `data/`:

- `phf_teams.rda`

### Parameters

Use `snake_case` for all function parameters:

- `game_id`, `team_abbr`, `season`, `player_id`

### Season Format Conventions

Different APIs use different season formats:

| Context                                                                                                                                    | Format                    | Example      |
|--------------------------------------------------------------------------------------------------------------------------------------------|---------------------------|--------------|
| NHL Web API / Stats API parameter                                                                                                          | Concatenated years string | `"20242025"` |
| [`most_recent_nhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_nhl_season.md) return value                     | Numeric end year          | `2025`       |
| [`most_recent_nhl_season_api_param()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_nhl_season_api_param.md) return value | Concatenated string       | `"20242025"` |
| PWHL / PHF season parameter                                                                                                                | Numeric year              | `2024`       |

## Testing

- **Framework:** testthat edition 3
- **Location:** `tests/testthat/`
- **File naming:** `test-<function_name>.R`

### Test Template

``` r
test_that("LEAGUE - Descriptive test name", {
    skip_on_cran()
    x <- function_name(param = value)

    expect_s3_class(x, "data.frame")
    expect_s3_class(x, "fastRhockey_data")
    expect_true(nrow(x) > 0)

    expected_cols <- c("col1", "col2")
    for (col in expected_cols) {
        expect_true(col %in% names(x), info = paste("Missing column:", col))
    }
})
```

### Test Conventions

- `skip_on_cran()` on all tests hitting live APIs
- Check both `data.frame` and `fastRhockey_data` S3 classes
- Validate `nrow(x) > 0`
- Check expected column names
- Use specific game IDs / dates for reproducibility
- xG tests: add `skip_if_not_installed("xgboost")`

### Testing Environment Variables

Tests in this package make **live API calls** and are skipped on CRAN
via `skip_on_cran()`. To run the full test suite locally:

- **`NOT_CRAN=true`**: Set this environment variable so that
  `skip_on_cran()` does not skip tests. You can set it in your
  `.Renviron` file or inline:

  ``` r
  Sys.setenv(NOT_CRAN = "true")
  devtools::test()
  ```

- **League-specific test toggles** (via `tests/testthat/helper-skip.R`):

  | Variable         | Default   | Controls                                 |
  |------------------|-----------|------------------------------------------|
  | `RUN_NHL_TESTS`  | `"true"`  | NHL API tests (`skip_nhl_test()`)        |
  | `RUN_PWHL_TESTS` | `"true"`  | PWHL API tests (`skip_pwhl_test()`)      |
  | `RUN_PHF_TESTS`  | `"false"` | PHF deprecated tests (`skip_phf_test()`) |

  Set them before running tests:

  ``` r
  Sys.setenv(NOT_CRAN = "true", RUN_NHL_TESTS = "true", RUN_PWHL_TESTS = "true")
  devtools::test()
  ```

  On CI, these are set in `.github/workflows/R-CMD-check.yaml` as:
  `NHL_TESTS=1`, `PWHL_TESTS=1`, `PHF_TESTS=0`.

- **`GITHUB_PAT`**: Some loader functions access data from GitHub
  repositories under the `sportsdataverse` org. Set a GitHub personal
  access token to avoid rate limits:

  ``` r
  # In ~/.Renviron
  GITHUB_PAT=ghp_your_token_here
  ```

- **Reproducible test data**: Use specific, known game IDs, dates, and
  seasons in tests (e.g., `game_id = 2023020001`, `season = 2024`)
  rather than `most_recent_*_season()` helpers, so test expectations
  remain stable across seasons.

## Conventional Commits

This project uses [Conventional
Commits](https://www.conventionalcommits.org/) for all commit messages.
The format is:

    <type>(<scope>): <description>

### Types

| Type       | When to Use                                                      |
|------------|------------------------------------------------------------------|
| `feat`     | A new feature or function                                        |
| `fix`      | A bug fix                                                        |
| `docs`     | Documentation-only changes                                       |
| `style`    | Formatting, whitespace, semicolons (no logic change)             |
| `refactor` | Code change that neither fixes a bug nor adds a feature          |
| `test`     | Adding or updating tests                                         |
| `build`    | Changes to build system or dependencies (DESCRIPTION, NAMESPACE) |
| `ci`       | Changes to CI configuration (GitHub Actions workflows)           |
| `chore`    | Maintenance tasks (version bumps, config updates)                |

### Scopes

| Scope     | Covers                         |
|-----------|--------------------------------|
| `nhl`     | NHL functions and helpers      |
| `phf`     | PHF functions (defunct league) |
| `pwhl`    | PWHL functions                 |
| `xg`      | Expected goals model pipeline  |
| `pkgdown` | pkgdown site and vignettes     |
| `ci`      | GitHub Actions and CI/CD       |
| `loader`  | Data loader functions          |

### Examples

    feat(pwhl): add pwhl_draft() function
    fix(nhl): handle null venue in schedule response
    test(pwhl): add tests for pwhl_standings
    docs(pkgdown): update reference index grouping
    build(nhl): add lifecycle to Imports
    ci: update R-CMD-check matrix to R 4.4
    refactor(nhl): extract common parsing into .parse_schedule()

## Deprecation Process

When a function needs to be deprecated (e.g., an API is retired or a
function is superseded by a better alternative), use the
[lifecycle](https://lifecycle.r-lib.org/) package:

### Step 1: Add the deprecation warning

``` r
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated. Use [new_function()] instead.
my_old_function <- function(param) {
    lifecycle::deprecate_warn(
        when = "1.0.0",
        what = "my_old_function()",
        with = "my_new_function()"
    )
    # Delegate to the new function
    my_new_function(param)
}
```

### Step 2: Update the roxygen2 tags

- Add the `lifecycle::badge("deprecated")` to `@description`
- Add `@seealso` pointing to the replacement function
- Keep the function exported so existing code does not break immediately

### Step 3: Update package infrastructure

- Ensure `lifecycle` is in the `Imports` field of `DESCRIPTION`
- Call
  [`usethis::use_lifecycle()`](https://usethis.r-lib.org/reference/use_lifecycle.html)
  if lifecycle infrastructure is not yet set up (adds the badge images
  and roxygen2 configuration)

### Step 4: Document in NEWS.md

Add a **Deprecations** section in `NEWS.md` listing every deprecated
function and its replacement.

## Reporting Issues

- Use the [bug report
  template](https://fastRhockey.sportsdataverse.org/ISSUE_TEMPLATE/bug_report.md)
  for bugs
- Use the [feature request
  template](https://fastRhockey.sportsdataverse.org/ISSUE_TEMPLATE/feature_request.md)
  for enhancements
- Include a minimal reproducible example when possible

## Code of Conduct

Be respectful and constructive in all interactions. This is a community
project maintained by volunteers.
