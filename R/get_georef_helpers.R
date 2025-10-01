#' Download BDTOPO helper layers for manual georeferencing
#'
#' @param x Object of class `sf` or `sfc`
#' @param ... **category names** to select which helper layers to download:
#'  * `"hydro"`: hydrography (nodes, river segments, lake, pond…)
#'  * `"road"`: roads and railway lines)
#'  * `"infra"`: linear / point / surface infrastructure
#'  * `"building"`: building footprints
#'  * `"other"`: miscellaneous landmarks (e.g. reference points)
#' @param filename `character` or `NULL`; optional path to a file. If supplied, all
#' downloaded layers are written to a single **GeoPackage** at this location.
#' Extension is automatically forced to `.gpkg`.
#' @param crs `numeric` or `character`; crs to convert layers to, see [sf::st_transform]
#'
#' @importFrom sf st_write st_transform
#' @importFrom happign get_wfs
#' @importFrom stats setNames
#'
#' @export
#' @return A **named list** of `sf` objects.
#'
#' @examples
#' \dontrun{
#' library(sf)
#' x <- read_sf(system.file("extdata/penmarch.shp", package = "happign"))
#' hydro <- get_georef_helpers(x, "hydro")
#' all <- get_georef_helpers(x)
#'
#' gpkg <- tempfile(fileext = ".gpkg")
#' save_to_gpkg <- get_georef_helpers(x, "road", "hydro", filename = gpkg)
#' }
#'
get_georef_helpers <- function(x, ..., filename = NULL, crs = 3857){
  lookup <- list(
    "hydro" = c("BDTOPO_V3:noeud_hydrographique", "BDTOPO_V3:troncon_hydrographique", "BDTOPO_V3:plan_d_eau"),
    "road" = c("BDTOPO_V3:troncon_de_route", "BDTOPO_V3:troncon_de_voie_ferree"),
    "infra" = c("BDTOPO_V3:construction_lineaire", "BDTOPO_V3:construction_ponctuelle", "BDTOPO_V3:construction_surfacique"),
    "building" = c("BDTOPO_V3:batiment"),
    "other" = c("BDTOPO_V3:point_de_repere")
  )

  cat <- c(...)

  # invalid cat
  invalid_cat <- setdiff(cat, names(lookup))
  if (length(invalid_cat) > 0) {
    message(sprintf(
      "Invalid category aren't used: '%s'", paste(invalid_cat, collapse = "', '")
    ))
  }

  # Valid key
  valid_cat <- intersect(cat, names(lookup))
  if (length(valid_cat) == 0){
    valid_cat <- names(lookup)
    message("No valid categories provided, all layers are downloaded.")
  }

  selected <- lookup[valid_cat] |> unlist(use.names = FALSE)

  layers <- lapply(selected, \(layer) get_wfs(x, layer)) |>
    setNames(gsub("BDTOPO_V3:","", selected))
  layers <- Filter(\(x) !is.null(x), layers) |>
    lapply(\(x) st_transform(x, crs))

  if (!is.null(filename)){
    force_gpkg <- paste0(tools::file_path_sans_ext(filename), ".gpkg")
    for (i in seq_along(layers)){
      st_write(layers[[i]], force_gpkg, layer = names(layers)[i])
    }
  }

  return(layers)
}

