test_that("get_photos(mode = 'raw') downloads and returns raw file paths", {

  source <- c("https://example.org/a.tif", "https://example.org/b.tif")
  outdir <- tempdir() |> file.path("delorean")
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(outdir, recursive = TRUE, force = TRUE))

  calls <- list(download = character())
  local_mocked_bindings(
    url_parser = function(x) list(image_identifier = sub(".*\\/([ab]).tif$", "\\1", x)),
    reverse_url_metadata = function(source) stop("should not be called in raw mode"),
    build_gcp_args = function(...) stop("should not be called in raw mode")
  )

  local_mocked_bindings(
    multi_download = function(urls, destfiles, ...) {
      calls$download <<- c(calls$download, urls)
      lapply(destfiles, function(x){
        dir.create(dirname(x), recursive = TRUE, showWarnings = FALSE)
        writeBin(as.raw(1:10), x)
      })
      return(data.frame(success = TRUE, destfile = destfiles, url = urls))
    },
    .package = "curl"
  )

  res <- get_photos(source, outdir = NULL, mode = "raw", quiet = TRUE, overwrite = TRUE)

  expect_type(res, "character")
  expect_length(res, 2)
  expect_true(all(file.exists(res)))
  expect_equal(basename(res), c("a_raw.tif", "b_raw.tif"))
  expect_equal(calls$download, source)
})

test_that("get_photos() warn when download failing", {

  source <- c("https://example.org/a.tif", "https://example.org/b.tif")
  outdir <- tempdir() |> file.path("delorean")
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(outdir, recursive = TRUE, force = TRUE))

  calls <- list(download = character())
  local_mocked_bindings(
    url_parser = function(x) list(image_identifier = sub(".*\\/([ab]).tif$", "\\1", x)),
    reverse_url_metadata = function(source) stop("should not be called in raw mode"),
    build_gcp_args = function(...) stop("should not be called in raw mode")
  )

  local_mocked_bindings(
    multi_download = function(urls, destfiles, ...) {
      calls$download <<- c(calls$download, urls)
      lapply(destfiles, function(x){
        dir.create(dirname(x), recursive = TRUE, showWarnings = FALSE)
        writeBin(as.raw(1:10), x)
      })
      return(data.frame(success = c(FALSE, TRUE), destfile = destfiles, url = urls))
    },
    .package = "curl"
  )

  expect_message(
    res <- get_photos(source, outdir = NULL, mode = "raw", quiet = FALSE, overwrite = TRUE),
    "1 download failed"
  ) |> suppressMessages()

  expect_type(res, "character")
  expect_length(res, 1)
  expect_true(all(file.exists(res)))
  expect_equal(basename(res), c("b_raw.tif"))
  expect_equal(calls$download, source)
})

test_that("get_photos() handle all failed url", {

  source <- c("https://example.org/a.tif", "https://example.org/b.tif")
  outdir <- tempdir() |> file.path("delorean")
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(outdir, recursive = TRUE, force = TRUE))

  calls <- list(download = character())
  local_mocked_bindings(
    url_parser = function(x) list(image_identifier = sub(".*\\/([ab]).tif$", "\\1", x)),
    reverse_url_metadata = function(source) stop("should not be called in raw mode"),
    build_gcp_args = function(...) stop("should not be called in raw mode")
  )

  local_mocked_bindings(
    multi_download = function(urls, destfiles, ...) {
      calls$download <<- c(calls$download, urls)
      lapply(destfiles, function(x){
        dir.create(dirname(x), recursive = TRUE, showWarnings = FALSE)
        writeBin(as.raw(1:10), x)
      })
      return(data.frame(success = c(FALSE), destfile = destfiles, url = urls))
    },
    .package = "curl"
  )

  expect_error(
    res <- get_photos(source, outdir = NULL, mode = "raw", quiet = FALSE, overwrite = TRUE),
    "All downloads failed."
  )

})

test_that("get_photos(mode = 'gcp') translates and returns final path (current behavior)", {

  outdir <- tempdir() |> file.path("delorean")
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(outdir, recursive = TRUE, force = TRUE))

  source <- c("https://example.org/a.tif")

  calls <- list(translate = 0L, warp = 0L)
  local_mocked_bindings(
    url_parser = function(x) list(image_identifier = "a"),
    build_gcp_args = function(...) c("-gcp", "0", "0", "1", "2"),
    reverse_url_metadata = function(source) {data.frame(
      url = source, x = 1, y = 2, resolution = 20000, orientation = 180, stringsAsFactors = FALSE
      )}
    )

  local_mocked_bindings(
    multi_download = function(urls, destfiles, ...) {
      lapply(destfiles, function(x){
        dir.create(dirname(x), recursive = TRUE, showWarnings = FALSE)
        writeBin(as.raw(1:10), x)
      })
      return(data.frame(success = TRUE, destfile = destfiles, url = urls))
    },
    .package = "curl"
  )

  local_mocked_bindings(
    translate = function(src, dst, ..., cl_arg = NULL, quiet = FALSE) {
      calls$translate <<- calls$translate + 1L
      # simulate output from GDAL translate
      writeBin(as.raw(11:20), dst)
      invisible(dst)
    },
    warp = function(...) {
      calls$warp <<- calls$warp + 1L
      stop("warp should not be called in gcp mode")
    },
    .package = "gdalraster"
  )

  res <- get_photos(source, outdir = outdir, mode = "gcp", quiet = TRUE, overwrite = TRUE)

  expect_type(res, "character")
  expect_true(file.exists(file.path(outdir, "a_raw.tif")))
  expect_true(file.exists(file.path(outdir, "a.tif")))
  expect_equal(calls$translate, 1L)
  expect_equal(calls$warp, 0L)
  expect_equal(res, file.path(outdir, "a.tif")) # current return behavior
})

test_that("get_photos(mode = 'warp') translates then warps and returns final path (current behavior)", {

  outdir <- tempdir() |> file.path("delorean")
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(outdir, recursive = TRUE, force = TRUE))

  source <- c("https://example.org/a.tif")

  calls <- list(translate = 0L, warp = 0L)
  local_mocked_bindings(
    url_parser = function(x) list(image_identifier = "a"),
    build_gcp_args = function(...) c("-gcp", "0", "0", "1", "2"),
    reverse_url_metadata = function(source) {data.frame(
      url = source, x = 1, y = 2, resolution = 20000, orientation = 180, stringsAsFactors = FALSE
    )}
  )

  local_mocked_bindings(
    multi_download = function(urls, destfiles, ...) {
      lapply(destfiles, function(x){
        dir.create(dirname(x), recursive = TRUE, showWarnings = FALSE)
        writeBin(as.raw(1:10), x)
      })
      return(data.frame(success = TRUE, destfile = destfiles, url = urls))
    },
    .package = "curl"
  )

  local_mocked_bindings(
    translate = function(src, dst, ..., cl_arg = NULL, quiet = FALSE) {
      calls$translate <<- calls$translate + 1L
      writeBin(as.raw(11:20), dst)
      invisible(dst)
    },
    warp = function(src, dst, ..., t_srs = NULL, cl_arg = NULL, quiet = FALSE) {
      calls$warp <<- calls$warp + 1L
      writeBin(as.raw(21:30), dst)
      invisible(dst)
    },
    .package = "gdalraster"
  )

  res <- get_photos(source, outdir = outdir, mode = "warp", quiet = TRUE, overwrite = TRUE)

  expect_true(file.exists(file.path(outdir, "a_raw.tif")))
  expect_true(file.exists(file.path(outdir, "a.tif")))
  expect_equal(calls$translate, 1L)
  expect_equal(calls$warp, 1L)
  expect_equal(res, file.path(outdir, "a.tif")) # current return behavior
})

test_that("get_photos(mode = 'gcp') skips processing when final exists and overwrite = FALSE", {

  outdir <- tempdir() |> file.path("delorean")
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(outdir, recursive = TRUE, force = TRUE))

  source <- c("https://example.org/a.tif")

  # Pre-create final file
  writeBin(as.raw(99:100), file.path(outdir, "a.tif"))

  calls <- list(translate = 0L)
  local_mocked_bindings(
    url_parser = function(x) list(image_identifier = "a"),
    reverse_url_metadata = function(source) {
      data.frame(url = source, x = 1, y = 2, resolution = 20000, orientation = 180)
    },
    build_gcp_args = function(...) stop("should not be called when skipping")
  )

  local_mocked_bindings(
    multi_download = function(urls, destfiles, ...) {
      lapply(destfiles, function(x){
        dir.create(dirname(x), recursive = TRUE, showWarnings = FALSE)
        writeBin(as.raw(1:10), x)
      })
      return(data.frame(success = TRUE, destfile = destfiles, url = urls))
    },
    .package = "curl"
  )

  local_mocked_bindings(
    translate = function(...) {
      calls$translate <<- calls$translate + 1L
      stop("translate should not be called when skipping")
    },
    .package = "gdalraster"
  )

  res <- get_photos(source, outdir = outdir, mode = "gcp", quiet = TRUE, overwrite = FALSE)

  expect_equal(calls$translate, 0L)
  expect_true(file.exists(file.path(outdir, "a.tif")))
  expect_equal(res, file.path(outdir, "a.tif")) # since your function returns last final_path variable
})
