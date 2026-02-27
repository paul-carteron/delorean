# Retrieve image metadata

Retrieve image metadata

## Usage

``` r
find_photos(x, year = NULL, color = c("P", "C", "IR", "IRC"), oblique = FALSE)
```

## Arguments

- x:

  Object of class `sf` or `sfc`

- year:

  `integer`; multiple years can be set with vector `c(1980, 1990)` or
  range `1980:2990`. If `NULL` all years available are returned

- color:

  `character`; One or many of `"P"` (classic photo), `"C"` (color, rgb
  image), `"IR"` (infra-red) or `"IRC"` (infra-red color). All by
  default.

- oblique:

  `logical`; `FALSE` for **vertical** aerial photos (camera pointing
  straight down), `TRUE` for **oblique** photos. Use `NA` or `NULL` to
  retrieve both type.

## Value

`sf` with photo extent and metadata

## Details

By default, `get_image_metadata` retrieve photo extent. Centroid of
photo are contained in the dataset as the form of `x_3857`, `y_3857`. To
leverage them you can replace actual geometry with this one. See
@examples.

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)
x <- read_sf(system.file("extdata/penmarch.shp", package = "happign"))
images_metadata <- find_photos(x, year = 1969)

# Access centroids
centroid <- st_as_sf(
  st_drop_geometry(images_metadata),
  coords = c("x_3857", "y_3857"),
  crs = st_crs(3857)
)
centroid <- st_transform(centroid, 4326)

plot(
  st_geometry(images_metadata[1,]),
  col = "grey80", border = "grey60",
  main = "Photo footprint and centroid"
 )
plot(st_geometry(centroid[1,]), col = "blue", pch = 20, add = TRUE)
plot(st_geometry(x), col = "red", add = TRUE)
} # }
```
