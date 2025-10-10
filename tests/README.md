# LandslideR Test Suite

This directory contains the test suite for the LandslideR package using the `testthat` framework.

## Running Tests

### From R Console

```r
# Install testthat if not already installed
install.packages("testthat")

# Run all tests
devtools::test()

# Or using testthat directly
testthat::test_dir("tests/testthat")
```

### From Command Line

```bash
R CMD check .
```

Or with devtools:

```bash
Rscript -e "devtools::test()"
```

## Test Structure

The test suite is organized into the following files:

### `test-utility-functions.R`
- Tests for `replaceNA()` - replacing NA values with zeros
- Tests for `replaceZero()` - replacing zero values with NA
- Tests for `get_resolution()` - calculating raster resolution
- Tests for inverse operations between replaceNA and replaceZero

### `test-particle-functions.R`
- Tests for `set_slide_to_particles()` - converting topography to particle system
- Tests for `get_particle_values()` - extracting particle values
- Tests for `get_particle_slip_values()` - extracting slip values
- Consistency tests for particle value extraction

### `test-UltradiscreteCA.R`
- Input validation tests for model initialization
- Tests for NULL checks on domain and slip_area
- Tests for positive value validation (rock_size, scarp_depth)
- Tests for type validation (dynamic_particles)
- Tests for iteration parameter validation
- Tests for successful model initialization and simulation

### `test-simulation-logic.R`
- Tests for `model.simulate_ultradiff_particles()` core algorithm
- Particle structure preservation tests
- Flow direction tests (high to low)
- Thickness parameter tests
- Edge cell handling tests
- Coordinate preservation tests
- Dynamic vs static particle behavior tests

### `test-integration.R`
- Full workflow tests from initialization to simulation
- File output operation tests with error handling
- Tests for different rock_size parameters
- Displacement calculation tests
- Volume calculation tests (m³ and km³)
- Deterministic simulation tests

## Test Coverage

The test suite covers:

- ✅ Input validation
- ✅ Utility functions
- ✅ Particle system creation and manipulation
- ✅ Core simulation algorithm
- ✅ File I/O operations with error handling
- ✅ Mathematical correctness (flow direction, conservation)
- ✅ Integration testing
- ✅ Edge cases and error conditions

## Dependencies

The tests require the following packages:
- `testthat` (>= 3.0.0)
- `marmap`
- `raster`
- `R6`

## Notes

- Tests use `skip_if_not_installed()` to gracefully skip tests when optional dependencies are missing
- All tests preserve the mathematical implementation of the Ultradiscrete Cellular Automata algorithm
- Integration tests use small test matrices to ensure fast execution
