# The xG models load on first use and are checked against the release MANIFEST
# (xg_model_meta.json: penalty-shot constant, version, sha256 per file), not hashes
# pinned in this package, so a retrain ships by publishing (fastRhockey#96 follow-up).
# Offline: download.file is mocked; the manifest fixture is the real published one.

manifest_fixture <- test_path("fixtures", "nhl", "xg_model_meta.json")

# A fake release: `files` maps asset name -> local source path; NULL = no network.
mock_release <- function(env) {
    function(url, destfile, ...) {
        env$calls <- c(env$calls, url)
        if (is.null(env$files)) stop("cannot open URL '", url, "'")
        src <- env$files[[basename(url)]]
        if (is.null(src)) stop("HTTP status was '404 Not Found'")
        file.copy(src, destfile, overwrite = TRUE)
        0L
    }
}

# A manifest naming the four files written into `dir` (one line of text each).
fake_assets <- function(dir, tag = "a") {
    paths <- file.path(dir, .xg_files)
    for (p in paths) writeLines(paste(tag, basename(p)), p)
    m <- list(xg_model_ps = 0.31, version = tag,
              files = as.list(stats::setNames(vapply(paths, cli::hash_file_sha256, ""), .xg_files)))
    mp <- file.path(dir, .xg_manifest_name)
    jsonlite::write_json(m, mp, auto_unbox = TRUE)
    c(stats::setNames(as.list(paths), .xg_files), stats::setNames(list(mp), .xg_manifest_name))
}

test_that("loading the package does not fetch the xG models", {
    expect_false(exists(".onLoad", envir = asNamespace("fastRhockey"), inherits = FALSE))
})

test_that("the published manifest names every file with a sha256 and the PS constant", {
    m <- .xg_read_manifest(manifest_fixture)
    expect_identical(names(m$files), .xg_files)
    expect_true(all(grepl("^[0-9a-f]{64}$", m$files)))
    expect_true(is.numeric(m$xg_model_ps) && m$xg_model_ps > 0 && m$xg_model_ps < 1)
})

test_that("a manifest without file hashes (the pre-manifest release) is refused", {
    old <- withr::local_tempfile(fileext = ".json")
    writeLines('{"xg_model_ps": 0.3202197}', old)
    expect_error(.xg_read_manifest(old), "not in the expected format")
})

test_that("a cached file matching the manifest is used without a download", {
    src <- withr::local_tempdir(); cache <- withr::local_tempdir()
    rel <- fake_assets(src)
    file.copy(rel[["feats_st.txt"]], cache)
    env <- new.env(); env$files <- rel
    local_mocked_bindings(download.file = mock_release(env), .package = "utils")
    pin <- cli::hash_file_sha256(rel[["feats_st.txt"]])
    expect_identical(.xg_file("feats_st.txt", pin, cache), file.path(cache, "feats_st.txt"))
    expect_null(env$calls)
})

test_that("a retrain reaches the cache: the new manifest evicts and replaces stale files", {
    old <- withr::local_tempdir(); new <- withr::local_tempdir(); cache <- withr::local_tempdir()
    for (p in fake_assets(old, "old")[.xg_files]) file.copy(p, cache)
    rel <- fake_assets(new, "new")
    env <- new.env(); env$files <- rel
    local_mocked_bindings(download.file = mock_release(env), .package = "utils")
    m <- .xg_manifest(cache)
    expect_identical(m$version, "new")
    for (f in .xg_files) {
        dest <- .xg_file(f, m$files[[f]], cache)
        expect_identical(cli::hash_file_sha256(dest), m$files[[f]])
    }
    expect_length(env$calls, 1L + length(.xg_files))
    expect_match(env$calls[[1]], "releases/download/nhl_xg_models/xg_model_meta.json$")
    expect_setequal(list.files(cache), c(.xg_files, .xg_manifest_name))  # no temp files left
})

test_that("bytes that do not match the manifest are refused, not cached", {
    src <- withr::local_tempdir(); cache <- withr::local_tempdir()
    rel <- fake_assets(src)
    env <- new.env(); env$files <- rel
    local_mocked_bindings(download.file = mock_release(env), .package = "utils")
    expect_error(.xg_file("xg_model_st.json", strrep("0", 64), cache), "does not match")
    expect_false(file.exists(file.path(cache, "xg_model_st.json")))
})

test_that("offline, the cached manifest is used; with none, the failure is reported", {
    src <- withr::local_tempdir(); cache <- withr::local_tempdir()
    rel <- fake_assets(src, "cached")
    env <- new.env(); env$files <- NULL
    local_mocked_bindings(download.file = mock_release(env), .package = "utils")
    expect_error(.xg_manifest(cache), "Could not fetch")
    file.copy(rel[[.xg_manifest_name]], cache)
    expect_identical(.xg_manifest(cache)$version, "cached")
})

test_that("an unreadable cache entry is a miss, not an error", {
    src <- withr::local_tempdir(); cache <- withr::local_tempdir()
    rel <- fake_assets(src)
    dir.create(file.path(cache, "feats_5v5.txt"))
    env <- new.env(); env$files <- rel
    local_mocked_bindings(download.file = mock_release(env), .package = "utils")
    pin <- cli::hash_file_sha256(rel[["feats_5v5.txt"]])
    dest <- .xg_file("feats_5v5.txt", pin, cache)
    expect_length(env$calls, 1L)
    expect_identical(cli::hash_file_sha256(dest), pin)
    expect_length(list.files(cache), 1L)
})
