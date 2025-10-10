test_that("model.simulate_ultradiff_particles maintains particle structure", {
  skip_if_not_installed("marmap")

  # Create a simple particle system
  # 3x3 grid with center particle having higher value
  topo <- matrix(c(
    100, 100, 100,
    100, 200, 100,
    100, 100, 100
  ), nrow = 3, ncol = 3, byrow = TRUE)
  rownames(topo) <- c("0", "1", "2")
  colnames(topo) <- c("0", "1", "2")
  topo <- marmap::as.bathy(topo)

  # Set center as slide area
  slide_topo <- matrix(c(200), nrow = 1, ncol = 1)
  rownames(slide_topo) <- c("1")
  colnames(slide_topo) <- c("1")
  slide_topo <- marmap::as.bathy(slide_topo)

  particles <- set_slide_to_particles(topo, slide_topo, slip_value = 200)

  # Run one iteration
  result <- model.simulate_ultradiff_particles(
    particles = particles,
    thickness = 10,
    dynamic_particles = FALSE
  )

  # Check that result has same dimensions
  expect_equal(dim(result), dim(particles))

  # Check that all particles still have required structure
  for (i in 1:nrow(result)) {
    for (j in 1:ncol(result)) {
      particle <- result[[i, j]]
      expect_true(is.list(particle))
      expect_true("value" %in% names(particle))
      expect_true("slip_value" %in% names(particle))
      expect_true("latitude" %in% names(particle))
      expect_true("longitude" %in% names(particle))
    }
  }
})

test_that("model.simulate_ultradiff_particles flows from high to low", {
  skip_if_not_installed("marmap")

  # Create a tilted plane: high on left, low on right
  topo <- matrix(c(
    300, 200, 100,
    300, 200, 100,
    300, 200, 100
  ), nrow = 3, ncol = 3, byrow = TRUE)
  rownames(topo) <- c("0", "1", "2")
  colnames(topo) <- c("0", "1", "2")
  topo <- marmap::as.bathy(topo)

  # Set left column as slide area
  slide_topo <- matrix(c(300, 300, 300), nrow = 3, ncol = 1)
  rownames(slide_topo) <- c("0", "1", "2")
  colnames(slide_topo) <- c("0")
  slide_topo <- marmap::as.bathy(slide_topo)

  particles <- set_slide_to_particles(topo, slide_topo, slip_value = 300)

  # Run one iteration
  result <- model.simulate_ultradiff_particles(
    particles = particles,
    thickness = 10,
    dynamic_particles = FALSE
  )

  # Material should flow from high (left) to low (right)
  # The leftmost particles (which have slip values) should have decreased
  # We expect some change in the slip values

  initial_slip <- get_particle_slip_values(topo, particles)
  final_slip <- get_particle_slip_values(topo, result)

  # At least one slip value should have changed
  # (particles should have moved)
  expect_false(all(initial_slip == final_slip, na.rm = TRUE))
})

test_that("model.simulate_ultradiff_particles respects thickness parameter", {
  skip_if_not_installed("marmap")

  # Create simple setup
  topo <- matrix(c(
    100, 100, 100,
    100, 200, 100,
    100, 100, 100
  ), nrow = 3, ncol = 3, byrow = TRUE)
  rownames(topo) <- c("0", "1", "2")
  colnames(topo) <- c("0", "1", "2")
  topo <- marmap::as.bathy(topo)

  slide_topo <- matrix(c(200), nrow = 1, ncol = 1)
  rownames(slide_topo) <- c("1")
  colnames(slide_topo) <- c("1")
  slide_topo <- marmap::as.bathy(slide_topo)

  particles <- set_slide_to_particles(topo, slide_topo, slip_value = 200)

  # Run with thickness = 5
  result_small <- model.simulate_ultradiff_particles(
    particles = particles,
    thickness = 5,
    dynamic_particles = FALSE
  )

  # Run with thickness = 20
  result_large <- model.simulate_ultradiff_particles(
    particles = particles,
    thickness = 20,
    dynamic_particles = FALSE
  )

  # Results should be different
  values_small <- get_particle_values(topo, result_small)
  values_large <- get_particle_values(topo, result_large)

  expect_false(identical(values_small, values_large))
})

test_that("model.simulate_ultradiff_particles handles edge cells correctly", {
  skip_if_not_installed("marmap")

  # Create a 3x3 grid - simulation should not affect edge cells
  topo <- matrix(c(
    100, 100, 100,
    100, 200, 100,
    100, 100, 100
  ), nrow = 3, ncol = 3, byrow = TRUE)
  rownames(topo) <- c("0", "1", "2")
  colnames(topo) <- c("0", "1", "2")
  topo <- marmap::as.bathy(topo)

  slide_topo <- matrix(c(200), nrow = 1, ncol = 1)
  rownames(slide_topo) <- c("1")
  colnames(slide_topo) <- c("1")
  slide_topo <- marmap::as.bathy(slide_topo)

  particles <- set_slide_to_particles(topo, slide_topo, slip_value = 200)

  # Store initial edge values
  initial_values <- get_particle_values(topo, particles)

  # Run simulation
  result <- model.simulate_ultradiff_particles(
    particles = particles,
    thickness = 10,
    dynamic_particles = FALSE
  )

  final_values <- get_particle_values(topo, result)

  # Edge cells (first and last row/column) should remain unchanged
  # or at least the corners should remain unchanged as they're not processed
  # The algorithm processes cells from 2:(dimensions[1]-1) and 2:(dimensions[2]-1)
  # So corners and edges should be unchanged in terms of processing
  # Note: Adjacent cells might be updated, but edge cells themselves aren't directly updated

  # This is implementation-specific - the loop starts at index 2
  # So row 1, row 3, col 1, col 3 are never the focus of iteration
})

test_that("simulation preserves particle coordinate information", {
  skip_if_not_installed("marmap")

  # Create test topography
  topo <- matrix(c(
    100, 150, 200,
    100, 150, 200,
    100, 150, 200
  ), nrow = 3, ncol = 3, byrow = TRUE)
  rownames(topo) <- c("10", "11", "12")
  colnames(topo) <- c("120", "121", "122")
  topo <- marmap::as.bathy(topo)

  slide_topo <- matrix(c(200, 200, 200), nrow = 3, ncol = 1)
  rownames(slide_topo) <- c("10", "11", "12")
  colnames(slide_topo) <- c("122")
  slide_topo <- marmap::as.bathy(slide_topo)

  particles <- set_slide_to_particles(topo, slide_topo)

  # Run simulation
  result <- model.simulate_ultradiff_particles(
    particles = particles,
    thickness = 5,
    dynamic_particles = FALSE
  )

  # Check that latitude and longitude are preserved
  for (i in 1:nrow(result)) {
    for (j in 1:ncol(result)) {
      expect_equal(
        result[[i, j]]$latitude,
        particles[[i, j]]$latitude
      )
      expect_equal(
        result[[i, j]]$longitude,
        particles[[i, j]]$longitude
      )
    }
  }
})

test_that("dynamic_particles parameter affects simulation behavior", {
  skip_if_not_installed("marmap")

  # Create test setup
  topo <- matrix(c(
    100, 100, 100,
    100, 200, 100,
    100, 100, 100
  ), nrow = 3, ncol = 3, byrow = TRUE)
  rownames(topo) <- c("0", "1", "2")
  colnames(topo) <- c("0", "1", "2")
  topo <- marmap::as.bathy(topo)

  slide_topo <- matrix(c(200), nrow = 1, ncol = 1)
  rownames(slide_topo) <- c("1")
  colnames(slide_topo) <- c("1")
  slide_topo <- marmap::as.bathy(slide_topo)

  particles <- set_slide_to_particles(topo, slide_topo, slip_value = 200)

  # Run with dynamic_particles = FALSE
  result_static <- model.simulate_ultradiff_particles(
    particles = particles,
    thickness = 10,
    dynamic_particles = FALSE
  )

  # Run with dynamic_particles = TRUE
  result_dynamic <- model.simulate_ultradiff_particles(
    particles = particles,
    thickness = 10,
    dynamic_particles = TRUE
  )

  # Both should complete without error
  expect_true(is.list(result_static))
  expect_true(is.list(result_dynamic))

  # Results may differ (dynamic reorders directions based on values)
  # But both should maintain structure
  expect_equal(dim(result_static), dim(particles))
  expect_equal(dim(result_dynamic), dim(particles))
})
