
# LandslideR

<!-- badges: start -->
<!-- badges: end -->

The LandslideR is a tool for modelling mass failures such as subaerial and submarine landslides, that is based on Ultradiscretized Cellular Automata derived from two-dimensional diffusion equation

## Installation

You can install the development version of LandslideR like so:

``` r
library(devtools)

devtools::install_github("JomaMinoza/LandslideR")
```

## Example

This is a basic example:

``` r
library(LandslideR)

data("nwbt.slide.area")
data("nwbt.slide.area.scarp")

# Model Initializiation
model <- UltradiscreteCA$new(
  domain = nwbt.slide.area,          # ASCII/NetCDF Format
  slip_area = nwbt.slide.area.scarp, # Assumed location of the landslide
  rock_size = 0.5 # unit in meters
)


## Run 100 Timesteps for Experiment
experiment <- model$simulate(iterations = 100)

```

### Plot Results

``` r
## Plot Slide for each iteration

model$save_plots(type = "slide", every_n = 10, save_location = "examples/plots/slide")

## Plot Flow of Deposits for each iteration

model$save_plots(type = "slip", every_n = 10, save_location = "examples/plots/slip")

## Plot Delta Changes on Topography for each iteration

model$save_plots(type = "delta", every_n = 10, save_location = "examples/plots/delta")

```
