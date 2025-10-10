#' R6 Class representing a
#'@name UltradiscreteCA
#'@title Ultradiscretized Cellular Automata Model for Landslides
#'
#'@description 'UltradiscreteCA' class implements the Ultradiscretized Cellular Automata
#'   rules derived from two dimensional diffusion equation
#'
#'@examples
#'\dontrun{
#'data("nwbt.slide.area")
#'data("nwbt.slide.area.scarp")
#'
#'# Model Initializiation
#'model <- UltradiscreteCA$new(
#'domain = nwbt.slide.area,          # ASCII/NetCDF Format
#'slip_area = nwbt.slide.area.scarp, # Assumed location of the landslide
#'rock_size = 0.5 # unit in meters
#')
#'}
#' @export
UltradiscreteCA <- R6::R6Class(
  'UltradiscreteCA',

  public = list(

    domain = NULL,
    slip_area = NULL,
    rock_size = NULL,
    scarp_depth = NULL,
    dynamic_particles = NULL,
    resolution = NULL,

    initialize = function(domain, slip_area, rock_size = 0.5, scarp_depth = 1000, dynamic_particles = TRUE) {
      # Input validation
      if (is.null(domain)) {
        stop("domain cannot be NULL")
      }
      if (is.null(slip_area)) {
        stop("slip_area cannot be NULL")
      }
      if (!is.numeric(rock_size) || rock_size <= 0) {
        stop("rock_size must be a positive numeric value")
      }
      if (!is.numeric(scarp_depth) || scarp_depth <= 0) {
        stop("scarp_depth must be a positive numeric value")
      }
      if (!is.logical(dynamic_particles)) {
        stop("dynamic_particles must be a logical value (TRUE or FALSE)")
      }

      self$domain = domain
      self$slip_area    = slip_area
      self$rock_size    = rock_size
      self$scarp_depth  = scarp_depth
      self$dynamic_particles = dynamic_particles
      self$resolution = get_resolution(self$domain)

      private$set_particles()
      private$get_particle_slip_values()

    },

    get_iteration = function(particle){
      return_particle <- private$get_iteration_results(particle)
    },

    simulate = function(iterations = 5, save = TRUE) {
      # Input validation
      if (!is.numeric(iterations) || iterations <= 0 || iterations != as.integer(iterations)) {
        stop("iterations must be a positive integer")
      }

      particle <- self$get_particles()
      private$results <- list(particle)

      for(iteration in 1:iterations){

        print(paste("Running Simulation - Iteration", iteration))

        particle  <- private$get_iteration_results(particle)
        private$results[[iteration + 1]]   <- particle
      }

      private$experiment_particles <- self$get_simulation_particles()
      private$experiment_slip_values <- self$get_simulation_slip_values()

      return(private$results)
    },


    get_simulation_particles = function(){

      simulation_particles <- list()

      simulations <- length(private$results)

      for(simulation in 1:simulations){
        print(paste("Collecting Particle Values of the Simulation from Iteration", simulation))

        particles <- get_particle_values(self$domain,
                                                     private$results[[simulation]])

        simulation_particles[[simulation]] <- particles
      }

      return(simulation_particles)
    },

    get_simulation_slip_values = function(){

      simulation_slip_values <- list()

      simulations <- length(private$results)

      for(simulation in 1:simulations){

        print(paste("Collecting Slip Values of the Simulation from Iteration", simulation))

        slip_values <- get_particle_slip_values(self$domain,
                                                            private$results[[simulation]])
        simulation_slip_values[[simulation]] <- slip_values

      }

      return(simulation_slip_values)
    },

    get_simulation_displacements = function(every_n = 5){
      displacements <- private$get_displacement_topos(every_n = every_n)
      private$get_validation_of_mass_conservation(every_n = every_n)

      return(displacements)
    },

    get_simulation_sliding_volumes = function(every_n = 5, km = F){
      volumes <- private$get_displacement_volume(every_n = every_n, km = km)

      return(volumes)
    },

    save_plots = function(type = "slide", every_n = 1, save_location = getwd(), ...){
      if(type == "slide"){
        private$plot_slide(every_n = every_n, save_location = save_location, ...)
      } else if (type == "slip"){
        private$plot_slip(every_n = every_n, save_location = save_location, ...)
      } else if (type == "delta"){
        private$plot_delta(every_n = every_n, save_location = save_location, ...)
      } else {
        private$plot_slide(every_n = every_n, save_location = save_location, ...)
      }
    },

    save_output = function(every_n = 5,
                          file_name_prefix = "slide",
                          folder_location = getwd(),
                          crs = '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0',
                          resolution = c(0.0041656,0.0041656)){

      private$save_displacements(every_n = every_n,
                                 file_name_prefix = file_name_prefix,
                                 folder_location = folder_location,
                                 crs = crs,
                                 resolution = resolution)

    },

    get_particles = function(){
      return(private$particles)
    },

    get_slip_values = function(){
      return(private$slip_values)
    }

  ),
  private = list(

    particles = NULL,
    slip_values = NULL,
    results    = NULL,

    experiment_particles = NULL,
    experiment_slip_values = NULL,

    set_particles = function(){
      private$particles <- set_slide_to_particles(topo = self$domain,
                                         slide_topo = self$slip_area,
                                         slip_value = self$scarp_depth)
    },

    get_particle_slip_values = function(){
      private$slip_values <- get_particle_slip_values(self$domain,
                                                                  private$particles)
    },

    get_iteration_results = function(particle){

      simulation_experiment <- model.simulate_ultradiff_particles(particles = particle,
                                                                  thickness = self$rock_size,
                                                                  dynamic_particles = self$dynamic_particles)

      return(simulation_experiment)

    },

    get_delta_results = function(){

      delta_results <- list()

      if(is.null(private$experiment_particles)){
        results <- self$get_simulation_slip_values()
      } else {
        results <- private$experiment_slip_values
      }

      for(result in 1:length(results)){
        delta_results[[result]] <- results[[result]] - self$domain
      }

      return(delta_results)

    },

    get_displacement_topos = function(every_n = 1){

      if(is.null(private$experiment_particles)){
        topos <- self$get_simulation_particles()
      } else {
        topos <- private$experiment_particles
      }

      displacements <- list()
      references    <- unique(seq(1, length(topos), every_n), length(topos))

      for(reference in 2:length(references)){


        left_index        <- references[reference]
        right_index       <- references[reference - 1]

        print(paste0("Retriving Slide Displacement after Iteration ", left_index))

        displacement <- topos[[left_index]] - topos[[right_index]]
        displacements[[reference - 1]] <- displacement

      }

      return(displacements)

    },

    get_displacement_volume = function(every_n = 1, km = F){

      if(is.null(private$experiment_particles)){
        topos <- self$get_simulation_slip_values()
      } else {
        topos <- private$experiment_slip_values
      }

      area <- self$resolution[1] * self$resolution[2]
      unit <- "m^3"

      references    <- unique(seq(1, length(topos), every_n), length(topos))

      if(km){
        area <- area / 1000000000
        unit <- "km^3"
      }

      displacement_volume <- list()


      for(reference in 2:length(references)){

        index             <- references[reference]
        displacement      <- topos[[index]]

        volume <- sum(displacement, na.rm = T) * area

        print(paste0("Retriving Sliding Volume after Iteration ", reference - 1, ": ", volume, " ", unit))
        displacement_volume[[reference]] <- volume


      }

      return(displacement_volume)

    },

    get_validation_of_mass_conservation = function(every_n = 1){

      displacements <- private$get_displacement_topos(every_n = every_n)
      topos <- private$experiment_particles
      references    <- unique(seq(1, length(topos) - 1, every_n), length(topos))

      displacement_volume <- list()

      for(reference in 1:length(references)){

        volume <- sum(displacements[[reference]])

        print(paste0("Validating Conservation of Mass after Iteration ", reference, ": ", volume, " m^3 changes"))
        displacement_volume[[reference]] <- volume

      }

      return(displacement_volume)

    },


    save_displacement_topography = function(topo, file_name, folder_location){
      tryCatch({
        # Check if folder exists, create if not
        if (!dir.exists(folder_location)) {
          dir.create(folder_location, recursive = TRUE)
          message(paste("Created directory:", folder_location))
        }

        topo %>%
          as.bathy() %>%
          replaceNA() %>%
          as.raster() %>%
          writeRaster(
            paste0(folder_location,"/",file_name,".asc"),
            format = "ascii",
            overwrite = TRUE
          )
      }, error = function(e) {
        stop(paste("Failed to save topography file:", file_name, "\nError:", e$message))
      })
    },

    save_displacements = function(every_n = 5,
                                  file_name_prefix = "slide",
                                  folder_location = getwd(),
                                  crs = '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0',
                                  resolution = NULL,
                                  ...){
      if(is.null(resolution)){
        resolution <- self$resolution
      }

      displacements <- private$get_displacement_topos(every_n = every_n)

      for(displacement in 1:length(displacements)){

        print(paste0("Saving Slide Displacement: ", displacement))

        topography             <- displacements[[displacement]]


        resampled_displacement <- get_resampled_topo(topo = topography,
                                                               crs = crs,
                                                               resolution = resolution,
                                                               ...)

        private$save_displacement_topography(topo = resampled_displacement,
                                             file_name = paste0(file_name_prefix,displacement),
                                             folder_location = folder_location)

      }
    },

    plot_delta = function(every_n = 1, save_location = getwd(), ...){
      values = private$get_delta_results()

      # Check if folder exists, create if not
      if (!dir.exists(save_location)) {
        dir.create(save_location, recursive = TRUE)
        message(paste("Created directory:", save_location))
      }

      simulations <- seq(1, length(values), every_n)

      for(simulation in 1:length(simulations)){
        tryCatch({
          plot(values[[simulations[simulation]]], sub = paste0("Iteration ", simulations[simulation]), ...)
          dev.copy(png,paste0(save_location,'/',simulations[simulation],'.png'))
          dev.off()
        }, error = function(e) {
          warning(paste("Failed to save plot for iteration", simulations[simulation], ":", e$message))
        })
      }

    },

    plot_slide = function(every_n = 1, save_location = getwd(), ...){

      if(is.null(private$experiment_particles)){
        values <- self$get_simulation_particles()
      } else {
        values <- private$experiment_particles
      }

      # Check if folder exists, create if not
      if (!dir.exists(save_location)) {
        dir.create(save_location, recursive = TRUE)
        message(paste("Created directory:", save_location))
      }

      simulations <- seq(1, length(values), every_n)

      for(simulation in 1:length(simulations)){
        tryCatch({
          plot(values[[simulations[simulation]]], sub = paste0("Iteration ", simulations[simulation]), ...)
          dev.copy(png,paste0(save_location,'/',simulations[simulation],'.png'))
          dev.off()
        }, error = function(e) {
          warning(paste("Failed to save plot for iteration", simulations[simulation], ":", e$message))
        })
      }

    },

    plot_slip = function(every_n = 1, save_location = getwd(), ...){

      if(is.null(private$experiment_particles)){
        values <- self$get_simulation_slip_values()
      } else {
        values <- private$experiment_slip_values
      }

      # Check if folder exists, create if not
      if (!dir.exists(save_location)) {
        dir.create(save_location, recursive = TRUE)
        message(paste("Created directory:", save_location))
      }

      simulations <- seq(1, length(values), every_n)

      for(simulation in 1:length(simulations)){
        tryCatch({
          plot(self$domain)

          plot(values[[simulations[simulation]]], sub = paste0("Iteration ", simulations[simulation]), image = TRUE, add = TRUE, ...)
          dev.copy(png,paste0(save_location,'/',simulations[simulation],'.png'))
          dev.off()
        }, error = function(e) {
          warning(paste("Failed to save plot for iteration", simulations[simulation], ":", e$message))
        })
      }

    }

  )
)

