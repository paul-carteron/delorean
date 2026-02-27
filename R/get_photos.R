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
#' @seealso [curl::multi_download()]
#'
#' @returns `character`; filepaths of each photos
#'
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
get_photos <- function(
    source,
    outdir = NULL,
    mode = "raw",
    quiet = FALSE,
    overwrite = FALSE
){

  if (!mode %in% c("raw", "gcp", "warp")) {
    cli::cli_abort(c(
      "x" = "Invalid {.arg mode}: {.val {mode}}.",
      "i" = "Must be one of {.val raw}, {.val gcp}, {.val warp}."
    ))
  }

  if (is.null(outdir)) {
    outdir <- tools::R_user_dir("delorean", "cache")
  }

  if (!dir.exists(outdir)) {
    dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
    if (!quiet) cli::cli_alert_info("Created directory {.file {outdir}}.")
  }

  # ---- DOWNLOAD PHASE ----

  ids <- url_parser(source)
  raw_files <- file.path(outdir, paste0(ids$image_identifier, "_raw.tif"))

  res <- curl::multi_download(
    urls = source,
    destfiles = raw_files,
    resume = TRUE,
    progress = !quiet
  )

  failed <- !res$success
  if (all(failed)){
    cli::cli_abort("All downloads failed.")
  }

  if (any(failed, na.rm = TRUE)) {
    cli::cli_alert_warning("{sum(failed, na.rm = TRUE)} download{?s} failed.")
  }

  success <- res[res$success, , drop = FALSE]
  if (nrow(success) == 0) {
    cli::cli_abort("No successful downloads.")
  }

  if (mode == "raw") {
    if (!quiet) cli::cli_alert_success("Download complete.")
    return(invisible(success$destfile))
  }

  # ---- PROCESSING PHASE ----

  source_metadata <- reverse_url_metadata(success$url)
  n <- nrow(success)
  final_files <- character(n)

  for (i in seq_len(n)) {

    raw_path <- success$destfile[i]
    filename <- basename(success$url[i])
    final_path <- file.path(outdir, filename)

    if (!quiet) cli::cli_alert_info(sprintf("Processing [%s/%s]", i, n))

    if (file.exists(final_path) && !overwrite) {
      final_files[i] <- final_path
      next
    }

    gcp_args <- tryCatch(
      build_gcp_args(
        source = raw_path,
        center_x = source_metadata$x[i],
        center_y = source_metadata$y[i],
        resolution = source_metadata$resolution[i],
        orientation = source_metadata$orientation[i]
      ),
      error = function(e) {
        cli::cli_abort(c(
          "x" = "Failed building GCP for {.file {basename(raw_path)}}.",
          "i" = conditionMessage(e)
        ))
      }
    )

    tmp_path <- tempfile(fileext = ".tif")

    tryCatch({

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

    }, error = function(e) {
      cli::cli_abort(c(
        "x" = "Processing failed for {.file {basename(raw_path)}}.",
        "i" = conditionMessage(e)
      ))
    })

    final_files[i] <- final_path

  }

  return(final_files)
}
