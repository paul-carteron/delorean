#' Parse dataset and image identifier from PVA URLs
#'
#'
#' @param urls Character vector of image URLs.
#'
#' @return A data.frame with columns `dataset_identifier` and `image_identifier`.
#'
#' @noRd
url_parser <- function(urls) {
  stopifnot(is.character(urls))

  res <- lapply(urls, function(url) {
    parts <- strsplit(url, "/")[[1]]
    n <- length(parts)

    if (n < 2) stop("URL does not look like a valid PVA URL")

    dataset_identifier <- parts[n - 1]
    image_identifier <- sub("\\.tif$", "", parts[n])

    list(dataset_identifier = dataset_identifier,
         image_identifier   = image_identifier)
  })

  res <- do.call(rbind, lapply(res, as.data.frame, stringsAsFactors = FALSE))

  return(res)
}

#' Find image metadata from URL
#'
#' @param urls Character vector of image URLs.
#'
#' @importFrom happign get_wfs
#' @importFrom sf st_drop_geometry
#' @return A data.frame (or `sf` without geometry) with columns:
#'   `image_identifier`, `resolution`, `orientation`, `x`, `y`, `url`.
#'
#' @noRd
reverse_url_metadata <- function(urls){
  ids <- url_parser(urls)
  dataset_identifier <- ids$dataset_identifier |> unique()
  dataset_filter <- sprintf(
    "dataset_identifier IN (%s)",
    paste0("'", dataset_identifier, "'", collapse = ", ")
  )
  mission <- get_wfs(x = NULL, "pva:dataset", ecql_filter = dataset_filter) |>
    st_drop_geometry() |>
    suppressMessages()

  image_identifier <- ids$image_identifier
  image_filter <- sprintf(
    "image_identifier IN (%s)",
    paste0("'", image_identifier, "'", collapse = ", ")
  )
  metadata <- get_wfs(x = NULL, "pva:image", ecql_filter = image_filter) |>
    st_drop_geometry() |>
    merge(mission[, c("dataset_identifier", "resolution")]) |>
    transform(url = urls) |>
    suppressMessages()

  return(
    metadata[, c("image_identifier", "resolution", "orientation", "x", "y", "url")]
  )
}
