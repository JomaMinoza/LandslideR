test_that("replaceNA replaces NA values with zeros", {
  # Test with vector
  x <- c(1, NA, 3, NA, 5)
  result <- replaceNA(x)
  expected <- c(1, 0, 3, 0, 5)
  expect_equal(result, expected)

  # Test with all NAs
  x_all_na <- c(NA, NA, NA)
  result_all_na <- replaceNA(x_all_na)
  expect_equal(result_all_na, c(0, 0, 0))

  # Test with no NAs
  x_no_na <- c(1, 2, 3)
  result_no_na <- replaceNA(x_no_na)
  expect_equal(result_no_na, x_no_na)

  # Test with matrix
  mat <- matrix(c(1, NA, 3, NA), nrow = 2)
  result_mat <- replaceNA(mat)
  expected_mat <- matrix(c(1, 0, 3, 0), nrow = 2)
  expect_equal(result_mat, expected_mat)
})

test_that("replaceZero replaces zero values with NA", {
  # Test with vector
  x <- c(1, 0, 3, 0, 5)
  result <- replaceZero(x)
  expected <- c(1, NA, 3, NA, 5)
  expect_equal(result, expected)

  # Test with all zeros
  x_all_zero <- c(0, 0, 0)
  result_all_zero <- replaceZero(x_all_zero)
  expect_true(all(is.na(result_all_zero)))

  # Test with no zeros
  x_no_zero <- c(1, 2, 3)
  result_no_zero <- replaceZero(x_no_zero)
  expect_equal(result_no_zero, x_no_zero)

  # Test with matrix
  mat <- matrix(c(1, 0, 3, 0), nrow = 2)
  result_mat <- replaceZero(mat)
  expected_mat <- matrix(c(1, NA, 3, NA), nrow = 2)
  expect_equal(result_mat, expected_mat)
})

test_that("replaceNA and replaceZero are inverse operations", {
  # Start with vector containing zeros
  x <- c(1, 0, 3, 0, 5)

  # replaceZero then replaceNA should give back zeros
  result <- x %>% replaceZero() %>% replaceNA()
  expect_equal(result, x)

  # Start with vector containing NAs
  y <- c(1, NA, 3, NA, 5)

  # replaceNA then replaceZero should give back NAs
  result_y <- y %>% replaceNA() %>% replaceZero()
  expect_equal(result_y, y)
})

test_that("get_resolution returns correct resolution in meters", {
  # Create a simple raster-like object
  # This test assumes we have marmap and raster loaded
  skip_if_not_installed("marmap")
  skip_if_not_installed("raster")

  # Create a bathymetric object with known resolution
  # Using a small test matrix
  test_data <- matrix(c(100, 200, 300, 400), nrow = 2, ncol = 2)
  rownames(test_data) <- c("-10", "-10.01")
  colnames(test_data) <- c("120", "120.01")

  bathy_obj <- marmap::as.bathy(test_data)

  # Get resolution
  resolution <- get_resolution(bathy_obj)

  # Check that resolution is numeric and has 2 elements (x and y)
  expect_type(resolution, "double")
  expect_length(resolution, 2)

  # Resolution should be positive
  expect_true(all(resolution > 0))

  # Resolution should be in meters (multiplied by 111111)
  # For 0.01 degree spacing, expect approximately 1111 meters
  expect_true(all(resolution > 1000))
  expect_true(all(resolution < 2000))
})
