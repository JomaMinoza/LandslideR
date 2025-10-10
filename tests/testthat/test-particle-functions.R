test_that("set_slide_to_particles creates correct particle structure", {
  skip_if_not_installed("marmap")

  # Create a simple test topography
  topo <- matrix(c(100, 200, 300, 400, 500, 600), nrow = 3, ncol = 2)
  rownames(topo) <- c("0", "1", "2")
  colnames(topo) <- c("0", "1")
  topo <- marmap::as.bathy(topo)

  # Create a slide area (subset of topo)
  slide_topo <- matrix(c(100, 200), nrow = 1, ncol = 2)
  rownames(slide_topo) <- c("0")
  colnames(slide_topo) <- c("0", "1")
  slide_topo <- marmap::as.bathy(slide_topo)

  # Convert to particles
  particles <- set_slide_to_particles(topo, slide_topo, slip_value = 1000)

  # Check dimensions
  expect_equal(dim(particles), c(3, 2))

  # Check particle structure
  expect_type(particles, "list")

  # Check that each particle has the required fields
  particle <- particles[[1, 1]]
  expect_true(is.list(particle))
  expect_true("latitude" %in% names(particle))
  expect_true("longitude" %in% names(particle))
  expect_true("value" %in% names(particle))
  expect_true("slip_value" %in% names(particle))

  # Check that slip_value is set correctly for slide area
  # Particle at position (1,1) should be in slide area
  expect_equal(particles[[1, 1]]$slip_value, 100)

  # Particles outside slide area should have NA slip_value
  expect_true(is.na(particles[[2, 1]]$slip_value))
  expect_true(is.na(particles[[3, 1]]$slip_value))
})

test_that("get_particle_values extracts values correctly", {
  skip_if_not_installed("marmap")

  # Create a simple test topography
  topo <- matrix(c(100, 200, 300, 400), nrow = 2, ncol = 2)
  rownames(topo) <- c("0", "1")
  colnames(topo) <- c("0", "1")
  topo <- marmap::as.bathy(topo)

  # Create particles
  slide_topo <- matrix(c(100), nrow = 1, ncol = 1)
  rownames(slide_topo) <- c("0")
  colnames(slide_topo) <- c("0")
  slide_topo <- marmap::as.bathy(slide_topo)

  particles <- set_slide_to_particles(topo, slide_topo)

  # Extract particle values
  result <- get_particle_values(topo, particles)

  # Check that result has same dimensions as topo
  expect_equal(dim(result), dim(topo))

  # Check that values match original topography
  expect_equal(result[1, 1], 100)
  expect_equal(result[2, 1], 200)
  expect_equal(result[1, 2], 300)
  expect_equal(result[2, 2], 400)
})

test_that("get_particle_slip_values extracts slip values correctly", {
  skip_if_not_installed("marmap")

  # Create a simple test topography
  topo <- matrix(c(100, 200, 300, 400), nrow = 2, ncol = 2)
  rownames(topo) <- c("0", "1")
  colnames(topo) <- c("0", "1")
  topo <- marmap::as.bathy(topo)

  # Create slide area covering top-left cell only
  slide_topo <- matrix(c(100), nrow = 1, ncol = 1)
  rownames(slide_topo) <- c("0")
  colnames(slide_topo) <- c("0")
  slide_topo <- marmap::as.bathy(slide_topo)

  particles <- set_slide_to_particles(topo, slide_topo, slip_value = 1000)

  # Extract slip values
  result <- get_particle_slip_values(topo, particles)

  # Check that result has same dimensions as topo
  expect_equal(dim(result), dim(topo))

  # Check that slip value is set for slide area
  expect_equal(result[1, 1], 100)

  # Check that slip value is NA outside slide area
  expect_true(is.na(result[2, 1]))
  expect_true(is.na(result[1, 2]))
  expect_true(is.na(result[2, 2]))
})

test_that("particle values remain consistent through extraction", {
  skip_if_not_installed("marmap")

  # Create a test topography
  original_topo <- matrix(c(50, 100, 150, 200, 250, 300), nrow = 3, ncol = 2)
  rownames(original_topo) <- c("0", "1", "2")
  colnames(original_topo) <- c("0", "1")
  topo <- marmap::as.bathy(original_topo)

  # Create slide area
  slide_topo <- matrix(c(50, 100), nrow = 2, ncol = 1)
  rownames(slide_topo) <- c("0", "1")
  colnames(slide_topo) <- c("0")
  slide_topo <- marmap::as.bathy(slide_topo)

  # Create particles
  particles <- set_slide_to_particles(topo, slide_topo)

  # Extract values and verify they match original
  extracted_values <- get_particle_values(topo, particles)

  # Values should match original topography
  expect_equal(as.vector(extracted_values), as.vector(original_topo))
})
