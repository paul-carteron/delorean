#' Convert map scale to ground
#'
#' The constant `2.54` is used to convert inches to centimeters,
#' since DPI (dots per inch) is expressed in inches while map
#' scales are typically expressed per centimeter.
#'
#' By default `dpi = 1200`, which is the standard scanning
#' used by IGN for digitizing historical aerial photos.
#'
#' @return A numeric value,  in meters per pixel.
#'
#' @noRd
#'
scale_to_res <- function(resolution, dpi) {
  px_per_cm <- dpi / 2.54
  m_per_px  <- (resolution / px_per_cm) * 0.01  # conversion cm -> m
  return(m_per_px)
}

#' Build Ground Control Point arguments for GDAL georeferencing
#'
#' @param source `character`; Path or URL to the raster image.
#' @param center_x `numeric`; X coordinate of the photo’s center (map units).
#' @param center_y `numeric`; Y coordinate of the photo’s center (map units).
#' @param resolution `numeric`; Scale denominator (e.g. `20000` for 1:20000).
#' @param orientation `numeric`; Orientation angle in degrees clockwise from North.
#' @param dpi `numeric`; Scan  in dots per inch. Default is `1200`,
#' which corresponds to the scanning standard for IGN’s historical
#' aerial photo collection.
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
    orientation,
    dpi = 1200){

  r <- new(GDALRaster, source, read_only = TRUE)
  nc <- r$getRasterXSize()
  nr <- r$getRasterYSize()
  r$close

  res_m <- scale_to_res(resolution, dpi)
  half_w <- nc * res_m / 2
  half_h <- nr * res_m / 2

  theta <- (360 - orientation) * pi / 180
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
