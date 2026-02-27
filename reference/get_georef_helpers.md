# Download BDTOPO helper layers for manual georeferencing

Download BDTOPO helper layers for manual georeferencing

## Usage

``` r
get_georef_helpers(x, ..., filename = NULL, crs = 3857)
```

## Arguments

- x:

  Object of class `sf` or `sfc`

- ...:

  **category names** to select which helper layers to download:

  - `"hydro"`: hydrography (nodes, river segments, lake, pond…)

  - `"road"`: roads and railway lines)

  - `"infra"`: linear / point / surface infrastructure

  - `"building"`: building footprints

  - `"other"`: miscellaneous landmarks (e.g. reference points)

- filename:

  `character` or `NULL`; optional path to a file. If supplied, all
  downloaded layers are written to a single **GeoPackage** at this
  location. Extension is automatically forced to `.gpkg`.

- crs:

  `numeric` or `character`; crs to convert layers to, see
  [sf::st_transform](https://r-spatial.github.io/sf/reference/st_transform.html)

## Value

A **named list** of `sf` objects.

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)
x <- read_sf(system.file("extdata/penmarch.shp", package = "happign"))
hydro <- get_georef_helpers(x, "hydro")
all <- get_georef_helpers(x)

gpkg <- tempfile(fileext = ".gpkg")
save_to_gpkg <- get_georef_helpers(x, "road", "hydro", filename = gpkg)
} # }
```
