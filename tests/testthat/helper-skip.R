# Live-network tests run only when their toggle is "1", and never on CRAN or CI.
# "true" (any case) is still accepted, because older checkouts, ~/.Renviron files and
# the pre-1.1.0 workflows set RUN_*_TESTS=true. The CI rule lives here, not in each
# test file: the workflow toggles are passed through from repository variables but
# do not make GitHub Actions hit live APIs (a loaded runner or a provider outage
# turned main red at random). Set them locally to run the live suites.
skip_live_test <- function(var, what) {
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  if (!tolower(Sys.getenv(var)) %in% c("1", "true")) {
    testthat::skip(paste0(what, " live tests skipped (set ", var, "=1 to enable)"))
  }
  invisible(TRUE)
}

skip_nhl_test <- function() skip_live_test("RUN_NHL_TESTS", "NHL")

skip_phf_test <- function() skip_live_test("RUN_PHF_TESTS", "PHF")

skip_pwhl_test <- function() skip_live_test("RUN_PWHL_TESTS", "PWHL")

skip_espn_test <- function() skip_live_test("RUN_ESPN_TESTS", "ESPN")

# European hockey families (del/icehl/eihl/metal, khl, vhl, shl, ha, liiga, sihf,
# nl, elh, magnus, espn mch/wch). The providers are throttled to one request per
# second per host; fixture tests always run.
skip_euro_test <- function() skip_live_test("RUN_EURO_TESTS", "European hockey")

# Slow NHL tests (one request per team) need RUN_SLOW_NHL_TESTS on top of RUN_NHL_TESTS.
skip_slow_nhl_test <- function() skip_live_test("RUN_SLOW_NHL_TESTS", "Slow NHL")
