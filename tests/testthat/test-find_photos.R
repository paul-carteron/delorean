# minimal sf poly
poly <- st_sfc(st_point(c(0, 0)), crs = 4326)

test_that("check_find_photos() rejects wrong x type", {
  # data.frame instead of sf
  expect_error(
    check_find_photos(data.frame(a = 1)),
    regexp = "must be an.*sf.*sfc"
  )
})

test_that("check_find_photos() rejects wrong year type", {
  expect_error(
    check_find_photos(poly, year = "1990"),
    regexp = "must be .*numeric.*NULL"
  )
})

test_that("check_find_photos() rejects wrong color", {
  expect_error(
    check_find_photos(poly, year = NULL, color = "XYZ"),
    regexp = "Invalid value.*color"
  )
})

test_that("check_find_photos() rejects wrong oblique input", {
  # invalid type
  expect_error(
    check_find_photos(poly, year = NULL, color = "IR", oblique = "yes"),
    regexp = "must be TRUE, FALSE"
  )

  # invalid length
  expect_error(
    check_find_photos(poly, year = NULL, color = "IR", oblique = c(TRUE, TRUE, FALSE)),
    regexp = "must be TRUE, FALSE"
  )

  # invalid NA
  expect_message(
    suppressWarnings(check_find_photos(poly, year = NULL, color = "IR", oblique = NA)),
    regexp = "retrieving all photos"
  )
})

with_mock_dir("find_photos", {
  test_that("find_photos() returns NULL when no photos", {
    expect_message(
      res <- find_photos(poly, year = 1800),
      regexp = "No data found"
    )
    expect_null(res)
  })

  test_that("find_photos() works", {
    # penmarch centroid
    x <- st_as_sfc("POINT (-4.369559 47.79967)", crs = 4326)
    photo <- find_photos(x, oblique = TRUE, year = 1921)

    expect_s3_class(photo, "sf")
    expect_true(nrow(photo) >= 1)

    # --- Metadata columns (optional but stable) ---
    expected_cols <- c("image_identifier", "dataset_identifier")
    expect_true(all(expected_cols %in% names(photo)))
    })
})


