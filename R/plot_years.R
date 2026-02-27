#' Plot available years for a given area
#'
#' @param x Object of class `sf` or `sfc`
#'
#' @returns data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' library(happign)
#' x <- get_apicarto_cadastre("29158")
#' photos <- plot_years(x)
#' }
#'
plot_years <- function(x) {
  photos <- find_photos(x)
  photo$years <- as.numeric(format(photo$date_cliche, "%Y"))

  years <- sort(unique(photos$year))
  tinyplot::tinytheme("clean2", mar = c(5, 4, 5, 2))
  tinyplot::tinyplot(~ year | couleur,
                     data = photos,
                     main = "Number of photo by year",
                     type = "barplot",
                     xaxt = "n",
                     xlab = "Year",
                     palette = "Dark2",
                     ylab = "",
                     legend = tinyplot::legend("bottom!", title = "Photo type"))

    tinyplot::text(
      seq_along(years), 0, labels = years, srt = 75,
      adj = c(1.2, 0.5), xpd = TRUE, cex = 1.1
    )

    tinyplot::text(
      x = seq_along(years),
      y = as.numeric(table(photos$year)),
      labels = as.numeric(table(photos$year)),
      pos = 3,
      cex = 0.7, xpd = TRUE,
      col = "black"
    )

  return(invisible(photos))
}
