#' Interactively select aerial photos to download
#'
#' Provides a simple console-based interactive workflow to help users select
#' a subset of photos returned by [find_photos()]. The user chooses: a **year**,
#' a **color type** and the number of photos to keep
#'
#' This function does **not** download files. It only returns the selected
#' rows (typically to be passed to [get_photos()]).
#'
#' @param photos An `sf` object or `data.frame` produced by [find_photos()].
#' @inheritParams get_photos
#'
#' @return Invisibly returns a subset of `photos` corresponding to the user
#'   selection. Returns `NULL` invisibly if the user quits.
#'
#' @details
#' Internal color codes are mapped as follows:
#'
#' - `"P"` -> `"PHOTO"` (panchromatic / black & white)
#' - `"C"` -> `"RGB"` (color)
#' - `"IR"`` -> `"IRC"` (infrared)
#' - `"IRC"`` -> `"IRC"` (infrared color)
#'
#' @examples
#' \dontrun{
#' x <- read_sf(system.file("extdata/penmarch.shp", package = "happign"))
#' photos <- find_photos(x)
#'
#' selection <- select_photos(photos)
#'
#' # Then download:
#' get_photos(selection$url, outdir = tempdir(), mode = "raw")
#' }
#'
#' @export
select_photos <- function(photos, outdir = NULL) {

  # Normalize to factors for stable menus
  photos <- photos |> sf::st_drop_geometry()
  photos$year <- as.factor(format(photos$date_cliche, "%Y"))
  photos$color <- factor(
    photos$couleur,
    levels = c("P", "C", "IR", "IRC"),
    labels = c("PHOTO", "RGB", "IRC", "IRC")
    )

  tab <- stats::xtabs(~ year + color, data = photos)

  # ---- Select year ----
  years <- rownames(tab)
  year_labels <- sprintf(
    "%s (PHOTO:%d RGB:%d IRC:%d)",
    years, tab[, "PHOTO"], tab[, "RGB"], tab[, "IRC"]
  )

  idx_year <- utils::menu(c(year_labels, "Quit"), title = "Choose a year")
  if (idx_year == 0 || idx_year == length(year_labels) + 1) return(invisible(NULL))
  year_sel <- years[idx_year]

  # ---- Select color ----
  colors <- colnames(tab)
  color_labels <- sprintf("%s: %d", colors, as.numeric(tab[year_sel, colors]))

  idx_color <- utils::menu(c(color_labels, "Back", "Quit"),
                           title = paste("Choose a color for year", year_sel))

  if (idx_color == 0 || idx_color == length(color_labels) + 2) return(invisible(NULL))

  if (idx_color == length(color_labels) + 1){
    return(select_photos(photos = photos, outdir = outdir))
  }

  color_sel <- colors[idx_color]

  # ---- Subset candidates ----
  cand <- photos[photos$year == year_sel & photos$color == color_sel, , drop = FALSE]
  n_cand <- nrow(cand)
  if (n_cand == 0) {
    message("No photos for that year/color.")
    return(invisible(NULL))
  }

  # ---- How many to download ----
  ans <- readline("How many photos to download? ('all' for all): ")
  ans <- trimws(ans)

  n_take <- if (ans == "" || tolower(ans) == "all") n_cand else suppressWarnings(as.integer(ans))
  if (is.na(n_take) || n_take < 1) {
    message("Invalid number. Aborting.")
    return(invisible(NULL))
  }
  n_take <- min(n_take, n_cand)

  return(invisible(cand[seq_len(n_take),]))
}
