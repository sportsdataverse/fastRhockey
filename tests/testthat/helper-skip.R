# Helper functions for conditionally skipping tests based on environment variables.
# Set the relevant environment variable to "true" to enable that test suite.
# Example: Sys.setenv(RUN_NHL_TESTS = "true")

skip_nhl_test <- function() {
    val <- Sys.getenv("RUN_NHL_TESTS", unset = "true")
    if (!identical(tolower(val), "true")) {
        skip("NHL tests skipped (set RUN_NHL_TESTS=true to enable)")
    }
    invisible(TRUE)
}

skip_phf_test <- function() {
    val <- Sys.getenv("RUN_PHF_TESTS", unset = "false")
    if (!identical(tolower(val), "true")) {
        skip("PHF tests skipped (set RUN_PHF_TESTS=true to enable)")
    }
    invisible(TRUE)
}

skip_pwhl_test <- function() {
    val <- Sys.getenv("RUN_PWHL_TESTS", unset = "true")
    if (!identical(tolower(val), "true")) {
        skip("PWHL tests skipped (set RUN_PWHL_TESTS=true to enable)")
    }
    invisible(TRUE)
}
