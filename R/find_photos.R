#' Retrieve image metadata
#'
#' @param x Object of class `sf` or `sfc`
#' @param year `integer`; multiple years can be set with vector `c(1980, 1990)`
#' or range `1980:2990`. If `NULL` all years available are returned
#' @param color `character`; One or many of `"P"` (black and white), `"C"` (color),
#' `"IR"` (infra-red) or `"IRC"` (infra-red color). All by default.
#' @param oblique `logical`; `FALSE` for **vertical** aerial photos
#' (camera pointing straight down), `TRUE` for **oblique**  photos.
#' @param ... Other arg pass to [happign::get_wfs()]
#'
#' @importFrom happign get_wfs
#' @importFrom sf st_drop_geometry
#'
#' @details
#' By default, `get_image_metadata` retrieve photo extent. Centroid of photo
#' are contained in the dataset as the form of `x_3857`, `y_3857`. To leverage
#' them you can replace actual geometry with this one. See @examples.
#'
#' @returns `sf` with photo extent and metadata
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#' x <- read_sf(system.file("extdata/penmarch.shp", package = "happign"))
#' images_metadata <- get_image_metadata(x, year = 1969)
#' images_centroid <- st_as_sf(
#'   st_drop_geometry(images_metadata),
#'   coords = c("x_3857", "y_3857"),
#'   crs = st_crs(3857)
#' )
#' }
#'
find_photos <- function(x, year = NULL, color = c("P", "C", "IR", "IRC"), oblique = FALSE, ...){
  mission <- get_wfs(x, "pva:dataset") |>
    st_drop_geometry() |>
    suppressMessages()
  mission[, c("id")] <- NULL

  image <- get_wfs(x, "pva:image") |>
    merge(mission) |>
    suppressMessages()
  image <- image[image$couleur %in% color, ]
  image <- image[image$oblique %in% oblique, ]

  if (!is.null(year)){
    image <- image[format(image$date_cliche, "%Y") %in% as.character(year), ]
  }

  if (nrow(image) == 0){
    stop("No data found.", call. = FALSE)
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
