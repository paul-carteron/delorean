# Download and georeference photos

Download and georeference photos

## Usage

``` r
get_photos(
  source,
  outdir = NULL,
  mode = "raw",
  quiet = FALSE,
  overwrite = FALSE
)
```

## Arguments

- source:

  `character`; url(s) build from
  [`find_photos()`](https://paul-carteron.github.io/delorean/reference/find_photos.md).

- outdir:

  `character` or `NULL`; Directory where photos will be saved. Defaults
  to `NULL`, in which case the package user overwrite directory
  ([`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html)) is
  used.

- mode:

  `character`; One of :

  - `"raw"` : download photos as-is, with no spatial reference.

  - `"gcp"` : download and attach Ground Control Points (GCPs). The
    photo is georeferenced but not resampled onto a regular grid.

  - `"warp"` : download, georeference, and warp using GDAL. The photo is
    resampled to a grid, ensuring compatibility with most GIS software.

- quiet:

  `logical`. If `TRUE`, suppress progress messages. Default to `FALSE`.

- overwrite:

  `logical`. If `TRUE`, existing files are overwritten. Default to
  `FALSE`.

## Value

`character`; filepaths of each photos

## Details

Georeferencing performed by `"gcp"` or `"warp"` is only a first
approximation, based on the scale and orientation metadata provided with
the scans. This positioning can be *significantly inaccurate*. For
precise work, users should refine the georeferencing manually in a GIS
software. To assist with manual georeferencing, see
[`get_georef_helpers()`](https://paul-carteron.github.io/delorean/reference/get_georef_helpers.md)

The affine transformation is used, which means no geometric deformation
is applied beyond shift, rotation, and scale. All georeferenced outputs
are written in **EPSG:3857 (Web Mercator)**.

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)
library(terra)
x <- read_sf(system.file("extdata/penmarch.shp", package = "happign"))
image_metadata <- find_photos(x, 1969)
tmp <- tempdir()
photo <- get_photos(image_metadata$url[1], tmp)

plot(rast(photo))
} # }
```
