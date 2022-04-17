#' get_particle_slip_values - Retrieve Slip Values from Model Experiment
#'
#' @param topo
#' @param particles
#'
#' @return
#' @export
#'
#' @examples
#'
#'
get_particle_slip_values <- function(topo, particles){
  dimensions <- dim(topo)
  resulting_particles <- topo

  for(lon in 1:dimensions[1]){
    for(lat in 1:dimensions[2]){
      resulting_particles[lon,lat] <- particles[[lon,lat]]$slip_value
    }
  }

  return(resulting_particles)
}
