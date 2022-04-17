devtools::load_all()

data("nwbt.slide.area")
data("nwbt.slide.area.scarp")

# Model Initializiation
model <- UltradiscreteCA$new(
  domain = nwbt.slide.area,          # ASCII/NetCDF Format
  slip_area = nwbt.slide.area.scarp, # Assumed location of the landslide
  rock_size = 0.5 # unit in meters
)

## Get Particles
particles <- model$get_particles()

## Get Slip Values
slip_values <- model$get_slip_values()

## Run 100 Timesteps for Experiment 1
experiment <- model$simulate(iterations = 100)

## Get Particles for each iteration
experiment.particles <- model$get_simulation_particles()

## Get Slip Values for each iteration
experiment.slip_values <- model$get_simulation_slip_values()

## Get Displacements for each iteration
experiment.displacements <- model$get_simulation_displacements(every_n = 10)

## Plot Model Results

model$save_plots(type = "slide", every_n = 10, save_location = "examples/plots/slide")

model$save_plots(type = "slip", every_n = 10, save_location = "examples/plots/slip")

model$save_plots(type = "delta", every_n = 10, save_location = "examples/plots/delta")

# Save Output
model$save_output(every_n = 10,
                  file_name_prefix = "slide",
                  folder_location = "examples/output/dtopo_files")
