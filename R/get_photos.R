#' Download and georeference photos
#'
#' @param source `character`; url(s) build from [delorean::find_photos()].
#' @param outdir `character` or `NULL`; Directory where photos will be saved.
#' Defaults to `NULL`, in which case the package user overwrite directory
#' ([tools::R_user_dir()]) is used.
#' @param mode `character`; One of :
#'   * `"raw"` : download photos as-is, with no spatial reference.
#'   * `"gcp"` : download and attach Ground Control Points (GCPs).
#'     The photo is georeferenced but not resampled onto a regular grid.
#'   * `"warp"` : download, georeference, and warp using GDAL.
#'     The photo is resampled to a grid, ensuring compatibility with most
#'     GIS software.
#' @param quiet `logical`. If `TRUE`, suppress progress messages. Default
#' to `FALSE`.
#' @param overwrite `logical`. If `TRUE`, existing files are overwritten.
#' Default to `FALSE`.
#'
#' @details
#' Georeferencing performed by `"gcp"` or `"warp"` is only a first approximation,
#' based on the scale and orientation metadata provided with the scans.
#' This positioning can be *significantly inaccurate*. For precise work,
#' users should refine the georeferencing manually in a GIS software.
#' To assist with manual georeferencing, see [delorean::get_georef_helpers()]
#'
#' The affine transformation is used, which means no geometric
#' deformation is applied beyond shift, rotation, and scale. All georeferenced
#' outputs are written in **EPSG:3857 (Web Mercator)**.
#'
#' @returns `character`; filepaths of each photos
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(terra)
#' x <- read_sf(system.file("extdata/penmarch.shp", package = "happign"))
#' image_metadata <- find_photos(x, 1969)
#' tmp <- tempdir()
#' photo <- get_photos(image_metadata$url[1], tmp)
#'
#' plot(rast(photo))
#' }
get_photos <- function(source, outdir = NULL, mode = "raw", quiet = FALSE, overwrite = FALSE){

  if (is.null(outdir)){
    outdir <- tools::R_user_dir("delorean", "cache")
  }

  if (!dir.exists(outdir)) {
    dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  }

  n <- length(source)
  raw_files <- character(n)
  h <- curl::new_handle()
  for (i in seq_along(source)){
    ids <- url_parser(source[i])
    filename <- paste0(ids$image_identifier, "_raw.tif")
    filepath <- file.path(outdir, filename)

    if (file.exists(filepath) && !overwrite) {
      if (!quiet) message("Already downloaded: ", basename(filepath))
      raw_files[i] <- filepath
      next
    }

    if (!quiet) message(sprintf("Downloading [%d/%d] ", i, n))
    curl::curl_download(source[i], filepath, quiet = quiet, handle = h)
    raw_files[i] <- filepath
  }

  if (mode == "raw") {
    return(invisible(raw_files))
  }

  files <- character(n)
  source_metadata <- reverse_url_metadata(source)
  for (i in seq_along(raw_files)){
    raw_path <- raw_files[i]
    ids <- url_parser(source[i])
    final_path <- file.path(outdir, paste0(ids$image_identifier, ".tif"))

    if (file.exists(final_path) && !overwrite) {
      if (!quiet) message("Skipping processing: ", basename(final_path))
      files[i] <- filepath
      next
    }

    if (!quiet) message(sprintf("Processing [%d/%d] ", i, n))

    gcp_args <- build_gcp_args(
      source = raw_path,
      center_x = source_metadata$x[i],
      center_y = source_metadata$y[i],
      resolution = source_metadata$resolution[i],
      orientation = source_metadata$orientation[i]
    )

    tmp_path <- tempfile(fileext = ".tif")

    gdalraster::translate(
      raw_path,
      tmp_path,
      cl_arg = c(
        "-a_srs", "EPSG:3857",
        gcp_args,
        "-co", "COMPRESS=LZW",
        "-co", "TILED=YES"
      ),
      quiet = quiet
    )

    if (mode == "warp") {

      gdalraster::warp(
        tmp_path,
        final_path,
        t_srs = "EPSG:3857",
        cl_arg = c(
          "-order", "1",
          "-r", "bilinear",
          "-co", "COMPRESS=LZW",
          "-co", "TILED=YES"
        ),
        quiet = quiet
      )

      unlink(tmp_path)

    } else {
      file.rename(tmp_path, final_path)
    }

    files[i] <- final_path
  }

  return(invisible(final_path))
}
