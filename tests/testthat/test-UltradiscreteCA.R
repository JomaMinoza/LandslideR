test_that("UltradiscreteCA initialization validates NULL domain", {
  skip_if_not_installed("marmap")

  # Create a slide area
  slide_topo <- matrix(c(100), nrow = 1, ncol = 1)
  rownames(slide_topo) <- c("0")
  colnames(slide_topo) <- c("0")
  slide_topo <- marmap::as.bathy(slide_topo)

  expect_error(
    UltradiscreteCA$new(domain = NULL, slip_area = slide_topo),
    "domain cannot be NULL"
  )
})

test_that("UltradiscreteCA initialization validates NULL slip_area", {
  skip_if_not_installed("marmap")

  # Create a domain
  topo <- matrix(c(100, 200, 300, 400), nrow = 2, ncol = 2)
  rownames(topo) <- c("0", "1")
  colnames(topo) <- c("0", "1")
  topo <- marmap::as.bathy(topo)

  expect_error(
    UltradiscreteCA$new(domain = topo, slip_area = NULL),
    "slip_area cannot be NULL"
  )
})

test_that("UltradiscreteCA initialization validates rock_size", {
  skip_if_not_installed("marmap")

  # Create test data
  topo <- matrix(c(100, 200, 300, 400), nrow = 2, ncol = 2)
  rownames(topo) <- c("0", "1")
  colnames(topo) <- c("0", "1")
  topo <- marmap::as.bathy(topo)

  slide_topo <- matrix(c(100), nrow = 1, ncol = 1)
  rownames(slide_topo) <- c("0")
  colnames(slide_topo) <- c("0")
  slide_topo <- marmap::as.bathy(slide_topo)

  # Negative rock_size
  expect_error(
    UltradiscreteCA$new(domain = topo, slip_area = slide_topo, rock_size = -1),
    "rock_size must be a positive numeric value"
  )

  # Zero rock_size
  expect_error(
    UltradiscreteCA$new(domain = topo, slip_area = slide_topo, rock_size = 0),
    "rock_size must be a positive numeric value"
  )

  # Non-numeric rock_size
  expect_error(
    UltradiscreteCA$new(domain = topo, slip_area = slide_topo, rock_size = "invalid"),
    "rock_size must be a positive numeric value"
  )
})

test_that("UltradiscreteCA initialization validates scarp_depth", {
  skip_if_not_installed("marmap")

  # Create test data
  topo <- matrix(c(100, 200, 300, 400), nrow = 2, ncol = 2)
  rownames(topo) <- c("0", "1")
  colnames(topo) <- c("0", "1")
  topo <- marmap::as.bathy(topo)

  slide_topo <- matrix(c(100), nrow = 1, ncol = 1)
  rownames(slide_topo) <- c("0")
  colnames(slide_topo) <- c("0")
  slide_topo <- marmap::as.bathy(slide_topo)

  # Negative scarp_depth
  expect_error(
    UltradiscreteCA$new(domain = topo, slip_area = slide_topo, scarp_depth = -100),
    "scarp_depth must be a positive numeric value"
  )

  # Zero scarp_depth
  expect_error(
    UltradiscreteCA$new(domain = topo, slip_area = slide_topo, scarp_depth = 0),
    "scarp_depth must be a positive numeric value"
  )
})

test_that("UltradiscreteCA initialization validates dynamic_particles", {
  skip_if_not_installed("marmap")

  # Create test data
  topo <- matrix(c(100, 200, 300, 400), nrow = 2, ncol = 2)
  rownames(topo) <- c("0", "1")
  colnames(topo) <- c("0", "1")
  topo <- marmap::as.bathy(topo)

  slide_topo <- matrix(c(100), nrow = 1, ncol = 1)
  rownames(slide_topo) <- c("0")
  colnames(slide_topo) <- c("0")
  slide_topo <- marmap::as.bathy(slide_topo)

  # Non-logical dynamic_particles
  expect_error(
    UltradiscreteCA$new(domain = topo, slip_area = slide_topo, dynamic_particles = "yes"),
    "dynamic_particles must be a logical value"
  )

  expect_error(
    UltradiscreteCA$new(domain = topo, slip_area = slide_topo, dynamic_particles = 1),
    "dynamic_particles must be a logical value"
  )
})

test_that("UltradiscreteCA initializes correctly with valid inputs", {
  skip_if_not_installed("marmap")

  # Create test data
  topo <- matrix(c(100, 200, 300, 400, 500, 600), nrow = 3, ncol = 2)
  rownames(topo) <- c("0", "1", "2")
  colnames(topo) <- c("0", "1")
  topo <- marmap::as.bathy(topo)

  slide_topo <- matrix(c(100), nrow = 1, ncol = 1)
  rownames(slide_topo) <- c("0")
  colnames(slide_topo) <- c("0")
  slide_topo <- marmap::as.bathy(slide_topo)

  # Create model
  model <- UltradiscreteCA$new(
    domain = topo,
    slip_area = slide_topo,
    rock_size = 0.5,
    scarp_depth = 1000,
    dynamic_particles = TRUE
  )

  # Check that model is created
  expect_true(R6::is.R6(model))

  # Check that fields are set correctly
  expect_equal(model$rock_size, 0.5)
  expect_equal(model$scarp_depth, 1000)
  expect_true(model$dynamic_particles)

  # Check that particles are initialized
  particles <- model$get_particles()
  expect_equal(dim(particles), c(3, 2))

  # Check that slip values are initialized
  slip_values <- model$get_slip_values()
  expect_equal(dim(slip_values), c(3, 2))
})

test_that("UltradiscreteCA simulate validates iterations parameter", {
  skip_if_not_installed("marmap")

  # Create test data
  topo <- matrix(c(100, 200, 300, 400), nrow = 2, ncol = 2)
  rownames(topo) <- c("0", "1")
  colnames(topo) <- c("0", "1")
  topo <- marmap::as.bathy(topo)

  slide_topo <- matrix(c(100), nrow = 1, ncol = 1)
  rownames(slide_topo) <- c("0")
  colnames(slide_topo) <- c("0")
  slide_topo <- marmap::as.bathy(slide_topo)

  model <- UltradiscreteCA$new(domain = topo, slip_area = slide_topo)

  # Negative iterations
  expect_error(
    model$simulate(iterations = -1),
    "iterations must be a positive integer"
  )

  # Zero iterations
  expect_error(
    model$simulate(iterations = 0),
    "iterations must be a positive integer"
  )

  # Non-integer iterations
  expect_error(
    model$simulate(iterations = 2.5),
    "iterations must be a positive integer"
  )

  # Non-numeric iterations
  expect_error(
    model$simulate(iterations = "five"),
    "iterations must be a positive integer"
  )
})

test_that("UltradiscreteCA simulate runs and returns results", {
  skip_if_not_installed("marmap")

  # Create test data
  topo <- matrix(c(100, 200, 300, 400), nrow = 2, ncol = 2)
  rownames(topo) <- c("0", "1")
  colnames(topo) <- c("0", "1")
  topo <- marmap::as.bathy(topo)

  slide_topo <- matrix(c(100), nrow = 1, ncol = 1)
  rownames(slide_topo) <- c("0")
  colnames(slide_topo) <- c("0")
  slide_topo <- marmap::as.bathy(slide_topo)

  model <- UltradiscreteCA$new(domain = topo, slip_area = slide_topo, rock_size = 0.5)

  # Run simulation with small number of iterations
  results <- suppressMessages(model$simulate(iterations = 2))

  # Check that results is a list
  expect_type(results, "list")

  # Should have initial state + 2 iterations = 3 results
  expect_length(results, 3)

  # Each result should be a particle matrix
  expect_equal(dim(results[[1]]), c(2, 2))
  expect_equal(dim(results[[2]]), c(2, 2))
  expect_equal(dim(results[[3]]), c(2, 2))
})
