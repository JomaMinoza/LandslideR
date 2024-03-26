model.simulate_ultradiff_particles <- function(particles, thickness, dynamic_particles){
  dimensions <- dim(particles)
  resulting_particles <- particles
  directions <- list(c(0,1), c(0,-1), c(1,0), c(-1,0), c(-1,-1), c(-1,1), c(1,1), c(1,-1))

  for(lon in 2:(dimensions[1]-1)){
    for(lat in 2:(dimensions[2]-1)){
      value <- particles[[lon,lat]]$value
      H_max <- value

      if(dynamic_particles){
        direction_values <- sapply(directions, function(direction) particles[[lon+direction[1],lat+direction[2]]]$value)

        # Reorder directions based on the values
        directions <- directions[order(direction_values, decreasing = TRUE)]
      }


      for(direction in directions){
        temp_value <- particles[[lon+direction[1],lat+direction[2]]]$value
        H_max <- max(H_max, temp_value, na.rm = TRUE)
      }

      if(is.finite(H_max) & is.finite(value) & value != H_max){
        for(direction in directions){
          if(max(particles[[lon+direction[1],lat+direction[2]]]$value,-1e10, na.rm = TRUE) == H_max){
            if(!isTRUE(is.na(particles[[lon+direction[1],lat+direction[2]]]$slip_value))){
              particles[[lon+direction[1],lat+direction[2]]]$value <- H_max - thickness
              particles[[lon+direction[1],lat+direction[2]]]$slip_value <- particles[[lon+direction[1],lat+direction[2]]]$slip_value - thickness
              particles[[lon,lat]]$value <- particles[[lon,lat]]$value + thickness
              particles[[lon,lat]]$slip_value <- ifelse(!isTRUE(is.na(particles[[lon,lat]]$slip_value)),resulting_particles[[lon,lat]]$value + thickness,thickness)

              resulting_particles[[lon+direction[1],lat+direction[2]]]$value <- H_max - thickness
              resulting_particles[[lon+direction[1],lat+direction[2]]]$slip_value <- resulting_particles[[lon+direction[1],lat+direction[2]]]$slip_value - thickness
              resulting_particles[[lon,lat]]$value <- resulting_particles[[lon,lat]]$value + thickness
              resulting_particles[[lon,lat]]$slip_value <- ifelse(!isTRUE(is.na(resulting_particles[[lon,lat]]$slip_value)),resulting_particles[[lon,lat]]$value + thickness,thickness)
            }
          }
        }
      }
    }
  }

  return(resulting_particles)
}
