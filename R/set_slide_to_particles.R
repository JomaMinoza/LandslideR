#' set_slide_to_particles - Convert Topography into Particle
#'
#' @param topo
#' @param slide_topo
#' @param slip_value
#'
#' @return
#' @export
#'
#' @examples
#'
set_slide_to_particles <- function(topo, slide_topo, slip_value = 1000){
  dimensions <- dim(topo)
  topo_particles <- matrix(list(), dimensions[1], dimensions[2])
  slide_x_0 <- getFirst(as.numeric(rownames(slide_topo)))
  slide_x_1 <- getLast(as.numeric(rownames(slide_topo)))
  slide_y_0 <- getFirst(as.numeric(colnames(slide_topo)))
  slide_y_1 <- getLast(as.numeric(colnames(slide_topo)))

  for(lon in 1:dimensions[1]){
    for(lat in 1:dimensions[2]){
      latitude <- as.numeric(rownames(topo)[lon])
      longitude <- as.numeric(colnames(topo)[lat])
      slip_latitude <- (slide_x_0 <= latitude) && (latitude <= slide_x_1)
      slip_longitude <- (slide_y_0 <= longitude) && (longitude <= slide_y_1)
      slip_value <-  ifelse((slip_longitude && slip_latitude), slide_topo[as.character(latitude), as.character(longitude)], NA)
      particle <- list(latitude=latitude, longitude=longitude, value=topo[lon,lat], slip_value=slip_value)
      topo_particles[[lon,lat]] <- particle
    }
  }
  return(topo_particles)

}
