#' Plot available years for a given area
#'
#' @param x Object of class `sf` or `sfc`
#'
#' @return data.frame
#' @export
#'
#' @examples
#' {
#' library(happign)
#' x <- get_apicarto_cadastre("29158")
#' photos <- plot_photos(x)
#' }
#'
plot_photos <- function(x) {
  photos <- find_photos(x)
  photos$years <- as.numeric(format(photos$date_cliche, "%Y"))

  years <- sort(unique(photos$year))
  tinyplot::tinytheme("clean2", mar = c(5, 4, 5, 2))
  tinyplot::tinyplot(~ years | couleur,
                     data = photos,
                     main = "Number of photo by year",
                     type = "barplot",
                     xaxt = "n",
                     xlab = "",
                     palette = "Dark2",
                     ylab = "",
                     legend = graphics::legend("bottom!", title = "Photo type"))

    graphics::text(
      seq_along(years), 0, labels = years, srt = 75,
      adj = c(1.2, 0.5), xpd = TRUE, cex = 1.1
    )

    graphics::text(
      x = seq_along(years),
      y = as.numeric(table(photos$year)),
      labels = as.numeric(table(photos$year)),
      pos = 3,
      cex = 0.7, xpd = TRUE,
      col = "black"
    )

  return(invisible(photos))
}
