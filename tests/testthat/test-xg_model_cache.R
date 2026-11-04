# The xG models load on first use, are pinned by sha256, and a stale cache is evicted
# (fastRhockey#96). Offline: the download is mocked with the real release meta file.

meta_fixture <- test_path("fixtures", "nhl", "xg_model_meta.json")

mock_download <- function(env) {
    function(url, destfile, ...) {
        env$calls <- c(env$calls, url)
        file.copy(env$src, destfile, overwrite = TRUE)
        0L
    }
}

test_that("loading the package does not fetch the xG models", {
    expect_false(exists(".onLoad", envir = asNamespace("fastRhockey"), inherits = FALSE))
})

test_that("a cached file with the pinned sha256 is used without a download", {
    dir <- withr::local_tempdir()
    file.copy(meta_fixture, file.path(dir, "xg_model_meta.json"))
    env <- new.env()
    env$src <- meta_fixture
    local_mocked_bindings(download.file = mock_download(env), .package = "utils")
    expect_identical(.xg_file("xg_model_meta.json", dir), file.path(dir, "xg_model_meta.json"))
    expect_null(env$calls)
})

test_that("a stale cached file is evicted and fetched from the nhl_xg_models release", {
    dir <- withr::local_tempdir()
    dest <- file.path(dir, "xg_model_meta.json")
    writeLines('{"xg_model_ps": 0.31, "version": "2025-01"}', dest)
    env <- new.env()
    env$src <- meta_fixture
    local_mocked_bindings(download.file = mock_download(env), .package = "utils")
    expect_identical(.xg_file("xg_model_meta.json", dir), dest)
    expect_length(env$calls, 1L)
    expect_match(env$calls, "releases/download/nhl_xg_models/xg_model_meta.json$")
    expect_identical(cli::hash_file_sha256(dest), .xg_pins[["xg_model_meta.json"]])
    expect_false(file.exists(paste0(dest, ".part")))
})

test_that("release bytes that no longer match the pin are refused, not cached", {
    dir <- withr::local_tempdir()
    retrained <- withr::local_tempfile(fileext = ".json")
    writeLines('{"xg_model_ps": 0.33}', retrained)
    env <- new.env()
    env$src <- retrained
    local_mocked_bindings(download.file = mock_download(env), .package = "utils")
    expect_error(.xg_file("xg_model_meta.json", dir), "Update fastRhockey")
    expect_false(file.exists(file.path(dir, "xg_model_meta.json")))
})

test_that("every pinned asset is a 64-hex sha256", {
    expect_setequal(names(.xg_pins), c("xg_model_5v5.json", "xg_model_st.json",
                                       "xg_model_meta.json", "feats_5v5.txt", "feats_st.txt"))
    expect_true(all(grepl("^[0-9a-f]{64}$", .xg_pins)))
})
