#' Build Ground Control Point arguments for GDAL georeferencing
#'
#' @param source `character`; Path or URL to the raster image.
#' @param center_x `numeric`; X coordinate of the photo’s center (map units).
#' @param center_y `numeric`; Y coordinate of the photo’s center (map units).
#' @param resolution `numeric`; Scale denominator (e.g. `20000` for 1:20000).
#' @param orientation `numeric`; Orientation angle in degrees clockwise from North.
#'
#' @importFrom gdalraster GDALRaster translate warp
#' @importFrom methods new
#'
#' @details
#' For georefencing i need to find position of each corner of my image :
#' - Centroid is retrieved from IGN metadata x_3857 and y_3857
#' - Resolution in m is calculated from image resolution and dpi
#' - Coordinate of point are calculated by multiplying pixle by resolution
#' - Rotation matrix is used to rotate each corner
#' - Real coordinate are then calculated from centroid
#' - GCP are build as :(idx_col, idx_rpw)
#'   - (0,0) : TL
#'   - (0, nr) : BL
#'   - (nc,0) : TR
#'   - (nc, nr) : BR
#'
#' @return Character vector of GCP arguments, ready to pass to
#' [gdalraster::translate()] via its `cl_arg` argument.
#'
#' @noRd
build_gcp_args <- function(
    source,
    center_x, center_y,
    resolution,
    orientation){

  r <- new(GDALRaster, source, read_only = TRUE)
  nc <- r$getRasterXSize()
  nr <- r$getRasterYSize()
  metadata <- r$getMetadata(band = 0, domain = "")
  x_res <- as.numeric(sub(".*XRESOLUTION=([0-9.]+).*", "\\1", metadata[1]))
  y_res <- as.numeric(sub(".*YRESOLUTION=([0-9.]+).*", "\\1", metadata[2]))

  black_in_rows <- vapply(c(nr/3, nr/2, 2 * nr/3), \(x){
    vals <- r$read(band = 1, xoff = 0, yoff = floor(x), xsize = nc, ysize = 1, out_xsize = nc, out_ysize = 1)
    black <- sum(vals == 0, na.rm = TRUE)
  }, 0L) |> mean() |> floor()

  black_in_cols <- vapply(c(nc/3, nc/2, 2 * nc/3), \(x){
    vals <- r$read(band = 1, xoff = floor(x), yoff = 0, xsize = 10, ysize = nr, out_xsize = 1, out_ysize = nr)
    black <- sum(vals == 0, na.rm = TRUE)
  }, 0L) |> mean() |> floor()

  r$close()

  half_w <- nc * (resolution / x_res) * 0.01 / 2 + max(black_in_rows, black_in_cols)
  half_h <- nr * (resolution / y_res) * 0.01 / 2 + max(black_in_rows, black_in_cols)

  theta <- (orientation - 180) * pi / 180
  M <- matrix(c(
    cos(theta), -sin(theta),
    sin(theta),  cos(theta)),
    2, 2, byrow = TRUE)

  offs <- list(
    TL = c(-half_w,  half_h),
    TR = c( half_w,  half_h),
    BL = c(-half_w, -half_h),
    BR = c( half_w, -half_h)
  )

  rot  <- lapply(offs, function(v) as.numeric(M %*% v))
  world <- lapply(rot, function(v) c(center_x + v[1], center_y + v[2]))

  gcp_args <- c(
    "-gcp", "0", "0",               world$TL,
    "-gcp", "0", as.character(nr-1), world$BL,
    "-gcp", as.character(nc), "0",   world$TR,
    "-gcp", as.character(nc), as.character(nr-1), world$BR
  )

  return(gcp_args)
}
