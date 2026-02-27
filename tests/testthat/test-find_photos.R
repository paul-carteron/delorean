x <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)

mission <- data.frame(
  id = 1:2,
  dataset_identifier = c("DS1", "DS2"),
  date_mission = as.Date(c("1997-04-10", "2000-01-01")),
  stringsAsFactors = FALSE
)

image <- sf::st_sf(
  dataset_identifier = c("DS1", "DS1", "DS2", "DS2"),
  image_identifier   = c("IM1", "IM2", "IM3", "IM4"),
  date_cliche        = as.Date(c("1997-04-10", "1997-06-01", "2000-05-10", "2000-05-11")),
  numero_image       = c("001", "002", "003", "004"),
  orientation        = c(180, 180, 90, 90),
  resolution         = c(20000, 20000, 10000, 10000),
  support            = c("film", "film", "film", "film"),
  oblique            = c(FALSE, TRUE, FALSE, FALSE),
  couleur            = c("P", "C", "IR", "IRC"),
  x                  = c(1, 2, 3, 4),
  y                  = c(10, 20, 30, 40),
  geometry           = rep(x, 4),
  stringsAsFactors   = FALSE
)

test_that("find_photos() returns filtered photos with url + x_3857/y_3857 + ordered columns", {

  local_mocked_bindings(
    get_wfs = function(x, layer, ...) {
      if (layer == "pva:dataset") return(mission)
      if (layer == "pva:image") return(image)
      stop("Unexpected layer: ", layer)
    },
    .package = "happign"
  )

  res <- find_photos(x, year = 1997, color = c("P", "C", "IR", "IRC"), oblique = FALSE)

  # Should keep only year 1997 and oblique == FALSE -> only IM1 (P)
  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 1L)
  expect_equal(res$image_identifier, "IM1")
  expect_equal(res$couleur, "P")
  expect_false(res$oblique)

  # url correctly built
  expect_equal(
    res$url,
    "https://data.geopf.fr/telechargement/download/pva/DS1/IM1.tif"
  )

  # x_3857/y_3857 set from x/y
  expect_equal(res$x_3857, res$x)
  expect_equal(res$y_3857, res$y)

  # column order
  expected_order <- c(
    "dataset_identifier", "date_mission", "image_identifier", "date_cliche",
    "numero_image", "orientation", "resolution", "support",
    "oblique", "couleur", "url", "x_3857", "y_3857", "geometry"
  )
  expect_equal(names(res), expected_order)
})

test_that("find_photos() returns all years when year = NULL", {

  local_mocked_bindings(
    get_wfs = function(x, layer, ...) {
      if (layer == "pva:dataset") return(mission)
      if (layer == "pva:image") return(image)
      stop("Unexpected layer: ", layer)
    },
    .package = "happign"
  )

  res <- find_photos(
    x,
    year = NULL,
    color = c("P", "C", "IR", "IRC"),
    oblique = FALSE
  )

  expect_s3_class(res, "sf")

  # Oblique filter removes IM2 only
  expect_equal(nrow(res), 3L)
  expect_equal(sort(res$image_identifier), c("IM1", "IM3", "IM4"))

  # All available years in non-oblique images should be returned
  expect_equal(
    sort(unique(format(res$date_cliche, "%Y"))),
    c("1997", "2000")
  )
})

test_that("find_photos() filters by color and oblique", {

  local_mocked_bindings(
    get_wfs = function(x, layer, ...) {
      if (layer == "pva:dataset") return(mission)
      if (layer == "pva:image") return(image)
      stop("Unexpected layer: ", layer)
    },
    .package = "happign"
  )

  # Only panchro photos, non-oblique => IM1 only (P, FALSE, 1997)
  res <- find_photos(x, year = NULL, color = "P", oblique = FALSE)
  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 1L)
  expect_equal(res$image_identifier, "IM1")
  expect_equal(res$couleur, "P")
  expect_false(res$oblique)

  # Only color photos, oblique => IM2 only (C, TRUE, 1997)
  res <- find_photos(x, year = NULL, color = "C", oblique = TRUE)
  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 1L)
  expect_equal(res$image_identifier, "IM2")
  expect_equal(res$couleur, "C")
  expect_true(res$oblique)

  # Infrared photos (IR + IRC), non-oblique => IM3 + IM4
  res <- find_photos(x, year = NULL, color = c("IR", "IRC"), oblique = FALSE)
  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 2L)
  expect_equal(sort(res$image_identifier), c("IM3", "IM4"))
  expect_true(all(res$couleur %in% c("IR", "IRC")))
  expect_true(all(res$oblique %in% FALSE))
})

test_that("find_photos() supports multiple years filtering", {

  local_mocked_bindings(
    get_wfs = function(x, layer, ...) {
      if (layer == "pva:dataset") return(mission)
      if (layer == "pva:image") return(image)
      stop("Unexpected layer: ", layer)
    },
    .package = "happign"
  )

  res <- find_photos(x, year = c(1997, 2000), color = c("P", "C", "IR", "IRC"), oblique = FALSE)

  # year filter keeps 1997 + 2000, oblique FALSE drops IM2 -> IM1, IM3, IM4
  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 3L)
  expect_equal(sort(res$image_identifier), c("IM1", "IM3", "IM4"))
  expect_true(all(format(res$date_cliche, "%Y") %in% c("1997", "2000")))
  expect_true(all(res$oblique %in% FALSE))

  # URL created for all rows
  expect_true(all(grepl("^https://data\\.geopf\\.fr/telechargement/download/pva/", res$url)))
  expect_true(all(endsWith(res$url, ".tif")))

  # Ordered columns still respected
  expected_order <- c(
    "dataset_identifier", "date_mission", "image_identifier", "date_cliche",
    "numero_image", "orientation", "resolution", "support",
    "oblique", "couleur", "url", "x_3857", "y_3857", "geometry"
  )
  expect_equal(names(res), expected_order)
})

test_that("find_photos() returns NULL when no data match", {

  local_mocked_bindings(
    get_wfs = function(x, layer, ...) {
      if (layer == "pva:dataset") return(mission)
      if (layer == "pva:image") return(image)
      stop("Unexpected layer: ", layer)
    },
    .package = "happign"
  )

  # No photos in 1980
  expect_warning(
    res <- find_photos(x, year = 1980, color = c("P", "C", "IR", "IRC"), oblique = FALSE),
    "No data found"
  )

  expect_null(res)
})

test_that("find_photos() queries dataset then image layers", {

  calls <- character()

  local_mocked_bindings(
    get_wfs = function(x, layer, ...) {
      calls <<- c(calls, layer)
      if (layer == "pva:dataset") return(mission)
      if (layer == "pva:image") return(image)
      stop("Unexpected layer: ", layer)
    },
    .package = "happign"
  )

  res <- find_photos(x, year = 1997, color = "P", oblique = FALSE)
  expect_s3_class(res, "sf")
  expect_equal(calls, c("pva:dataset", "pva:image"))
})

