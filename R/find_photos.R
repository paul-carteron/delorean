#' Retrieve image metadata
#'
#' @param x Object of class `sf` or `sfc`
#' @param year `integer`; multiple years can be set with vector `c(1980, 1990)`
#' or range `1980:2990`. If `NULL` all years available are returned
#' @param color `character`; One or many of `"P"` (classic photo), `"C"` (color, rgb image),
#' `"IR"` (infra-red) or `"IRC"` (infra-red color). All by default.
#' @param oblique `logical`; `FALSE` for **vertical** aerial photos
#' (camera pointing straight down), `TRUE` for **oblique**  photos. Use `NA`
#' or `NULL` to retrieve both type.
#'
#' @importFrom happign get_wfs
#' @importFrom sf st_drop_geometry
#'
#' @details
#' By default, `get_image_metadata` retrieve photo extent. Centroid of photo
#' are contained in the dataset as the form of `x_3857`, `y_3857`. To leverage
#' them you can replace actual geometry with this one. See @examples.
#'
#' @return `sf` with photo extent and metadata
#'
#' @examples
#' {
#' library(sf)
#' x <- read_sf(system.file("extdata/penmarch.shp", package = "happign")) |>
#'   st_transform(4326)
#' images_metadata <- find_photos(x, year = 1969) |> st_transform(4326)
#'
#' # Access centroids
#' centroid <- st_as_sf(
#'   st_drop_geometry(images_metadata),
#'   coords = c("x_3857", "y_3857"),
#'   crs = st_crs(3857)
#' )
#' centroid <- st_transform(centroid, 4326)
#'
#' plot(
#'   st_geometry(images_metadata[1,]) ,
#'   col = "grey80", border = "grey60",
#'   main = "Photo footprint and centroid"
#'  )
#' plot(st_geometry(centroid[1,]), col = "blue", pch = 20, add = TRUE)
#' plot(st_geometry(x), col = "red", add = TRUE)
#' }
#'
#' @export
find_photos <- function(x, year = NULL, color = c("P", "C", "IR", "IRC"), oblique = FALSE){

  check_find_photos(x, year, color, oblique)

  mission <- happign::get_wfs(x, "pva:dataset", predicate = happign::intersects(), verbose = FALSE) |>
    st_drop_geometry() |>
    suppressMessages() |>
    suppressWarnings()
  mission[, c("id")] <- NULL

  image <- happign::get_wfs(x, "pva:image", predicate = happign::intersects(), verbose = FALSE) |>
    merge(mission) |>
    suppressMessages() |>
    suppressWarnings()
  image <- image[image$couleur %in% color, ]
  image <- image[image$oblique %in% oblique, ]

  if (!is.null(year)){
    image <- image[format(image$date_cliche, "%Y") %in% as.character(year), ]
  }

  image <- unique(image)
  if (nrow(image) == 0){
    cli::cli_warn("No data found, {.val NULL} is returned")
    return(NULL)
  }

  base_url <- "https://data.geopf.fr/telechargement/download/pva/%s/%s.tif"
  image$url <- sprintf(base_url, image$dataset_identifier, image$image_identifier)

  image$x_3857 <- image$x
  image$y_3857 <- image$y

  order <- c(
    "dataset_identifier", "date_mission", "image_identifier", "date_cliche",
    "numero_image", "orientation", "resolution", "support",
    "oblique", "couleur", "url", "x_3857", "y_3857", "geometry"
  )

  image <- image[, c(order)]

  return(image)
}

#' @noRd
check_find_photos <- function(x, year, color, oblique) {
  # ---- Check x ------------------------------------------------------------
  if (!inherits(x, c("sf", "sfc"))) {
    cli::cli_abort("{.arg x} must be an {.cls sf} or {.cls sfc} object.")
  }

  # ---- Check year ---------------------------------------------------------
  if (!is.null(year)) {
    if (!is.numeric(year)) {
      cli::cli_abort("{.arg year} must be {.cls numeric} or {.cls NULL}.")
    }
  }

  # ---- Check color --------------------------------------------------------
  valid_colors <- c("P", "C", "IR", "IRC")
  bad <- setdiff(color, valid_colors)
  if (length(bad) > 0) {
    cli::cli_abort(c(
      "Invalid value{?s} in {.arg color}: {.val {bad}}",
      "Valid choices are: {.val {valid_colors}}"
    ))
  }

  # ---- Check oblique ------------------------------------------------------
  # NULL or NA == retrieve all
  if (is.null(oblique) || (is.logical(oblique) && all(is.na(oblique)))) {
    oblique <- c(TRUE, FALSE)
    cli::cli_alert_info("`oblique` is NULL/NA, retrieving all photos (oblique + vertical).")
  }

  if (!is.logical(oblique) || !(length(oblique) %in% c(1L, 2L))) {
    cli::cli_abort("{.arg oblique} must be TRUE, FALSE, or c(TRUE, FALSE).")
  }

}
