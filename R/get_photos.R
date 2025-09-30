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
get_photos <- function(source, outdir = NULL, mode = "raw", quiet = FALSE, overwrite = TRUE){

  if (is.null(outdir)){
    outdir <- tools::R_user_dir("delorean", "cache")
  }

  try_georef <- mode %in% c("gcp", "warp")
  if (try_georef) {
    metadata <- reverse_url_metadata(source)
  }

  n <- length(source)
  files <- character(n)

  for (i in seq_along(source)) {
    ids <- url_parser(source[i])
    filename <- paste0(ids$image_identifier, ".tif")
    filepath <- file.path(outdir, filename)

    if (file.exists(filepath) && !overwrite) {
      if (!quiet) message("Skipping ", filename)
      files[i] <- filepath
      next
    }

    if (!quiet) cat(sprintf("Downloading [%d/%d] ", i, n))
    args <- c("-of", "GTiff", "-co", "COMPRESS=LZW", "-co", "TILED=YES")

    if (try_georef) {
      gcp_args <- build_gcp_args(
        source = metadata$url[i],
        center_x = metadata$x[i],
        center_y = metadata$y[i],
        resolution = metadata$resolution[i],
        orientation = metadata$orientation[i]
      )
      args <- c(args, "-a_srs", "EPSG:3857", gcp_args)
    }

    out <- if (mode == "warp") tempfile(fileext = ".vrt") else filepath
    translate(source[i], out, cl_arg = args, quiet = quiet)

    if (mode == "warp"){
      args_warp <- c(
        "-order", "1",
        "-r", "bilinear",
        "-co", "COMPRESS=LZW",
        "-co", "TILED=YES"
      )
      if (!quiet) cat(sprintf("Warping [%d/%d] ", i, n))
      warp(out, filepath, t_srs = "EPSG:3857", cl_arg = args_warp)
    }

    files[i] <- filepath

  }

  return(invisible(files))
}
