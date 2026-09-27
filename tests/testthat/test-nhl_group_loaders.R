# Offline tests stub csv_from_url() with rows copied verbatim from the published
# nhl_groups release assets; the last test is the gated live smoke call
# against the release itself.

groups_fixture <- function(text) {
    seen <- character()
    local_mocked_bindings(
        csv_from_url = function(input, ...) {
            seen <<- c(seen, input)
            data.table::fread(text = text, ...)
        },
        .env = parent.frame()
    )
    function() seen
}

test_that("NHL - load_nhl_team_group_seasons builds end-year urls and keeps contract dtypes", {
    skip_on_cran()
    seen <- groups_fixture(paste(
        "league,season,team_id,team_id_source,team_name,subdivision_id,conference_id,division_id,source,sources_agree,notes",
        "nhl,2014,5,espn,Detroit Red Wings,,nhl:eastern,nhl:atlantic,nhl,true,nhl_team_id=17",
        sep = "\n"
    ))
    x <- load_nhl_team_group_seasons(seasons = 2014)

    expect_equal(basename(seen()), "nhl_team_group_seasons_2014.csv")
    expect_match(seen(), "/releases/download/nhl_groups/", fixed = TRUE)
    expect_s3_class(x, "fastRhockey_data")
    expect_type(x$team_id, "character")
    expect_equal(x$team_id, "5")
    expect_type(x$season, "integer")
    expect_type(x$subdivision_id, "character")
    expect_true(is.na(x$subdivision_id))
    expect_type(x$sources_agree, "logical")
})

test_that("NHL - load_nhl_group_aliases reads the season-less file", {
    skip_on_cran()
    seen <- groups_fixture(paste(
        "league,group_id,source,source_id,name_kind,value,valid_from,valid_to",
        "nhl,nhl:atlantic,nhl,,abbreviation,A,2014,",
        sep = "\n"
    ))
    x <- load_nhl_group_aliases()

    expect_equal(basename(seen()), "nhl_group_aliases.csv")
    expect_type(x$source_id, "character")
    expect_true(is.na(x$source_id))
    expect_type(x$valid_from, "integer")
    expect_true(is.na(x$valid_to))
})

test_that("NHL - seasons = TRUE reads the all-seasons file", {
    skip_on_cran()
    seen <- groups_fixture(paste(
        "league,season,team_id,team_id_source,team_name,subdivision_id,conference_id,division_id,source,sources_agree,notes",
        "nhl,2014,5,espn,Detroit Red Wings,,nhl:eastern,nhl:atlantic,nhl,true,nhl_team_id=17",
        sep = "\n"
    ))
    load_nhl_team_group_seasons(seasons = TRUE)
    expect_equal(basename(seen()), "nhl_team_group_seasons.csv")
})

test_that("NHL - group loaders reject seasons before 1918", {
    skip_on_cran()
    expect_error(load_nhl_team_group_seasons(seasons = 1917))
})

test_that("NHL - a failed group download warns and returns the contract columns", {
    skip_on_cran()
    local_mocked_bindings(csv_from_url = function(...) stop("HTTP status was '404 Not Found'"))
    expect_warning(x <- load_nhl_groups(), "404")
    expect_equal(nrow(x), 0)
    expect_equal(
        colnames(x),
        c("league", "group_id", "level", "first_season", "last_season", "notes")
    )
})

test_that("NHL - Load NHL team group seasons (live): Detroit to the Atlantic in 2014", {
    skip_on_cran()
    skip_nhl_test()
    x <- load_nhl_team_group_seasons(seasons = 2013:2014)

    expect_s3_class(x, "fastRhockey_data")
    expect_setequal(unique(x$season), c(2013L, 2014L))
    red_wings <- x[x$team_id == "5", ]
    expect_equal(red_wings$division_id[order(red_wings$season)], c("nhl:norris-central", "nhl:atlantic"))
})

test_that("NHL - group loaders reject fractional seasons", {
    skip_on_cran()
    expect_error(load_nhl_team_group_seasons(seasons = 2014.5))
})
