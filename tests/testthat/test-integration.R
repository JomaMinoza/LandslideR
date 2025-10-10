test_that("full workflow runs from initialization to simulation", {
  skip_if_not_installed("marmap")
  skip_if_not_installed("raster")

  # Create a realistic test scenario
  # 5x5 grid with a slide area in the center
  topo_data <- matrix(c(
    100, 100, 100, 100, 100,
    100, 150, 150, 150, 100,
    100, 150, 200, 150, 100,
    100, 150, 150, 150, 100,
    100, 100, 100, 100, 100
  ), nrow = 5, ncol = 5, byrow = TRUE)

  rownames(topo_data) <- c("0", "1", "2", "3", "4")
  colnames(topo_data) <- c("0", "1", "2", "3", "4")
  topo <- marmap::as.bathy(topo_data)

  # Center cell as slide area
  slide_data <- matrix(c(200), nrow = 1, ncol = 1)
  rownames(slide_data) <- c("2")
  colnames(slide_data) <- c("2")
  slide_topo <- marmap::as.bathy(slide_data)

  # Initialize model
  model <- UltradiscreteCA$new(
    domain = topo,
    slip_area = slide_topo,
    rock_size = 5,
    scarp_depth = 200,
    dynamic_particles = TRUE
  )

  # Verify model is initialized
  expect_true(R6::is.R6(model))

  # Run simulation with few iterations
  results <- suppressMessages(model$simulate(iterations = 3))

  # Check results structure
  expect_type(results, "list")
  expect_length(results, 4)  # Initial + 3 iterations

  # Get simulation particles
  sim_particles <- suppressMessages(model$get_simulation_particles())
  expect_length(sim_particles, 4)

  # Get simulation slip values
  sim_slip_values <- suppressMessages(model$get_simulation_slip_values())
  expect_length(sim_slip_values, 4)

  # Each result should be a raster-like object with correct dimensions
  for (i in seq_along(sim_particles)) {
    expect_equal(dim(sim_particles[[i]]), c(5, 5))
    expect_equal(dim(sim_slip_values[[i]]), c(5, 5))
  }
})

test_that("model handles file output operations with error handling", {
  skip_if_not_installed("marmap")
  skip_if_not_installed("raster")

  # Create test data
  topo_data <- matrix(c(
    100, 150, 100,
    150, 200, 150,
    100, 150, 100
  ), nrow = 3, ncol = 3, byrow = TRUE)

  rownames(topo_data) <- c("0", "1", "2")
  colnames(topo_data) <- c("0", "1", "2")
  topo <- marmap::as.bathy(topo_data)

  slide_data <- matrix(c(200), nrow = 1, ncol = 1)
  rownames(slide_data) <- c("1")
  colnames(slide_data) <- c("1")
  slide_topo <- marmap::as.bathy(slide_data)

  model <- UltradiscreteCA$new(
    domain = topo,
    slip_area = slide_topo,
    rock_size = 5,
    scarp_depth = 200
  )

  # Run minimal simulation
  suppressMessages(model$simulate(iterations = 2))

  # Create temporary directory for output
  temp_dir <- tempdir()
  output_dir <- file.path(temp_dir, "landslide_test_output")

  # Directory shouldn't exist yet
  expect_false(dir.exists(output_dir))

  # Try to save output - should create directory automatically
  expect_message(
    model$save_output(
      every_n = 1,
      file_name_prefix = "test_slide",
      folder_location = output_dir
    ),
    "Created directory"
  )

  # Directory should now exist
  expect_true(dir.exists(output_dir))

  # Clean up
  unlink(output_dir, recursive = TRUE)
})

test_that("simulation with different rock_size produces different results", {
  skip_if_not_installed("marmap")

  # Create test topography
  topo_data <- matrix(c(
    100, 100, 100,
    100, 200, 100,
    100, 100, 100
  ), nrow = 3, ncol = 3, byrow = TRUE)

  rownames(topo_data) <- c("0", "1", "2")
  colnames(topo_data) <- c("0", "1", "2")
  topo <- marmap::as.bathy(topo_data)

  slide_data <- matrix(c(200), nrow = 1, ncol = 1)
  rownames(slide_data) <- c("1")
  colnames(slide_data) <- c("1")
  slide_topo <- marmap::as.bathy(slide_data)

  # Model with small rock size
  model_small <- UltradiscreteCA$new(
    domain = topo,
    slip_area = slide_topo,
    rock_size = 1,
    dynamic_particles = FALSE
  )

  # Model with large rock size
  model_large <- UltradiscreteCA$new(
    domain = topo,
    slip_area = slide_topo,
    rock_size = 10,
    dynamic_particles = FALSE
  )

  # Run same number of iterations
  results_small <- suppressMessages(model_small$simulate(iterations = 2))
  results_large <- suppressMessages(model_large$simulate(iterations = 2))

  # Get final states
  particles_small <- suppressMessages(model_small$get_simulation_particles())
  particles_large <- suppressMessages(model_large$get_simulation_particles())

  # Results should be different
  expect_false(identical(
    particles_small[[length(particles_small)]],
    particles_large[[length(particles_large)]]
  ))
})

test_that("get_simulation_displacements calculates changes correctly", {
  skip_if_not_installed("marmap")

  # Create test data
  topo_data <- matrix(c(
    100, 100, 100,
    100, 200, 100,
    100, 100, 100
  ), nrow = 3, ncol = 3, byrow = TRUE)

  rownames(topo_data) <- c("0", "1", "2")
  colnames(topo_data) <- c("0", "1", "2")
  topo <- marmap::as.bathy(topo_data)

  slide_data <- matrix(c(200), nrow = 1, ncol = 1)
  rownames(slide_data) <- c("1")
  colnames(slide_data) <- c("1")
  slide_topo <- marmap::as.bathy(slide_data)

  model <- UltradiscreteCA$new(
    domain = topo,
    slip_area = slide_topo,
    rock_size = 5
  )

  # Run simulation
  suppressMessages(model$simulate(iterations = 4))

  # Get displacements
  displacements <- suppressMessages(model$get_simulation_displacements(every_n = 2))

  # Should have displacement calculations
  expect_type(displacements, "list")
  expect_true(length(displacements) > 0)
})

test_that("get_simulation_sliding_volumes calculates volumes", {
  skip_if_not_installed("marmap")

  # Create test data
  topo_data <- matrix(c(
    100, 100, 100,
    100, 200, 100,
    100, 100, 100
  ), nrow = 3, ncol = 3, byrow = TRUE)

  rownames(topo_data) <- c("0", "1", "2")
  colnames(topo_data) <- c("0", "1", "2")
  topo <- marmap::as.bathy(topo_data)

  slide_data <- matrix(c(200), nrow = 1, ncol = 1)
  rownames(slide_data) <- c("1")
  colnames(slide_data) <- c("1")
  slide_topo <- marmap::as.bathy(slide_data)

  model <- UltradiscreteCA$new(
    domain = topo,
    slip_area = slide_topo,
    rock_size = 5
  )

  # Run simulation
  suppressMessages(model$simulate(iterations = 4))

  # Get volumes in cubic meters
  volumes_m3 <- suppressMessages(model$get_simulation_sliding_volumes(every_n = 2, km = FALSE))

  # Get volumes in cubic kilometers
  volumes_km3 <- suppressMessages(model$get_simulation_sliding_volumes(every_n = 2, km = TRUE))

  # Both should return lists
  expect_type(volumes_m3, "list")
  expect_type(volumes_km3, "list")

  # Should have same number of elements
  expect_equal(length(volumes_m3), length(volumes_km3))

  # Each element should be numeric
  for (i in seq_along(volumes_m3)) {
    expect_type(volumes_m3[[i]], "double")
    expect_type(volumes_km3[[i]], "double")
  }
})

test_that("multiple simulations on same model produce consistent results", {
  skip_if_not_installed("marmap")

  # Create test data
  topo_data <- matrix(c(
    100, 100, 100,
    100, 200, 100,
    100, 100, 100
  ), nrow = 3, ncol = 3, byrow = TRUE)

  rownames(topo_data) <- c("0", "1", "2")
  colnames(topo_data) <- c("0", "1", "2")
  topo <- marmap::as.bathy(topo_data)

  slide_data <- matrix(c(200), nrow = 1, ncol = 1)
  rownames(slide_data) <- c("1")
  colnames(slide_data) <- c("1")
  slide_topo <- marmap::as.bathy(slide_data)

  # Create two identical models
  model1 <- UltradiscreteCA$new(
    domain = topo,
    slip_area = slide_topo,
    rock_size = 5,
    scarp_depth = 200,
    dynamic_particles = FALSE
  )

  model2 <- UltradiscreteCA$new(
    domain = topo,
    slip_area = slide_topo,
    rock_size = 5,
    scarp_depth = 200,
    dynamic_particles = FALSE
  )

  # Run same simulation
  results1 <- suppressMessages(model1$simulate(iterations = 3))
  results2 <- suppressMessages(model2$simulate(iterations = 3))

  # Results should be identical (deterministic)
  expect_equal(length(results1), length(results2))

  # Compare final states
  particles1 <- suppressMessages(model1$get_simulation_particles())
  particles2 <- suppressMessages(model2$get_simulation_particles())

  # Final particle values should be identical
  expect_equal(
    particles1[[length(particles1)]],
    particles2[[length(particles2)]]
  )
})
