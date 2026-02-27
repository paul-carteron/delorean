# Interactively select aerial photos to download

Provides a simple console-based interactive workflow to help users
select a subset of photos returned by
[`find_photos()`](https://paul-carteron.github.io/delorean/reference/find_photos.md).
The user chooses: a **year**, a **color type** and the number of photos
to keep

## Usage

``` r
select_photos(photos, outdir = NULL)
```

## Arguments

- photos:

  An `sf` object or `data.frame` produced by
  [`find_photos()`](https://paul-carteron.github.io/delorean/reference/find_photos.md).

- outdir:

  `character` or `NULL`; Directory where photos will be saved. Defaults
  to `NULL`, in which case the package user overwrite directory
  ([`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html)) is
  used.

## Value

Invisibly returns a subset of `photos` corresponding to the user
selection. Returns `NULL` invisibly if the user quits.

## Details

This function does **not** download files. It only returns the selected
rows (typically to be passed to
[`get_photos()`](https://paul-carteron.github.io/delorean/reference/get_photos.md)).

Internal color codes are mapped as follows:

- `"P"` -\> `"PHOTO"` (panchromatic / black & white)

- `"C"` -\> `"RGB"` (color)

- ``` "IR"`` ->  ```"IRC"\` (infrared)

- ``` "IRC"`` ->  ```"IRC"\` (infrared color)

## Examples

``` r
if (FALSE) { # \dontrun{
x <- sf::read_sf("my_area.gpkg")
photos <- find_photos(x)

sel <- select_photos(photos)

# Then download:
get_photos(sel$url, outdir = tempdir(), mode = "raw")
} # }
```
