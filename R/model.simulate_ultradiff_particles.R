model.simulate_ultradiff_particles <- function(particles, thickness){
  dimensions <- dim(particles)
  resulting_particles <- particles

  for(lon in 2:(dimensions[1]-1)){
    for(lat in 2:(dimensions[2]-1)){

      value <- particles[[lon,lat]]$value
      maxY = max(particles[[lon,lat+1]]$value,particles[[lon,lat-1]]$value, na.rm = TRUE)
      maxX = max(particles[[lon+1,lat]]$value,particles[[lon-1,lat]]$value, na.rm = TRUE)

      maxY2 = max(particles[[lon+1,lat+1]]$value,particles[[lon-1,lat-1]]$value, na.rm = TRUE)
      maxX2 = max(particles[[lon-1,lat+1]]$value,particles[[lon+1,lat-1]]$value, na.rm = TRUE)

      H_max = max(value, maxY, maxX, maxY2, maxX2, na.rm = TRUE)

      if(is.finite(H_max) & is.finite(value)){
        if(value == H_max){
          next
        }

        else{
          if (max(particles[[lon,lat-1]]$value,-1e10, na.rm = TRUE)==H_max){
            if(!isTRUE(is.na(particles[[lon,lat-1]]$slip_value))){
              particles[[lon,lat-1]]$value      <- H_max - thickness
              particles[[lon,lat-1]]$slip_value <- particles[[lon,lat-1]]$slip_value - thickness
              particles[[lon,lat]]$value        <- particles[[lon,lat]]$value + thickness
              particles[[lon,lat]]$slip_value   <- ifelse(!isTRUE(is.na(particles[[lon,lat]]$slip_value)),resulting_particles[[lon,lat]]$value + thickness,thickness)

              resulting_particles[[lon,lat-1]]$value      <- H_max - thickness
              resulting_particles[[lon,lat-1]]$slip_value <- resulting_particles[[lon,lat-1]]$slip_value - thickness
              resulting_particles[[lon,lat]]$value        <- resulting_particles[[lon,lat]]$value + thickness
              resulting_particles[[lon,lat]]$slip_value   <- ifelse(!isTRUE(is.na(resulting_particles[[lon,lat]]$slip_value)),resulting_particles[[lon,lat]]$value + thickness,thickness)
            }
          }
          else if (max(particles[[lon,lat+1]]$value,-1e10, na.rm = TRUE)==H_max){
            if(!isTRUE(is.na(particles[[lon,lat+1]]$slip_value))){
              particles[[lon,lat+1]]$value      <- H_max - thickness
              particles[[lon,lat+1]]$slip_value <- particles[[lon,lat+1]]$slip_value - thickness
              particles[[lon,lat]]$value        <- particles[[lon,lat]]$value + thickness
              particles[[lon,lat]]$slip_value   <- ifelse(!isTRUE(is.na(particles[[lon,lat]]$slip_value)),resulting_particles[[lon,lat]]$value + thickness,thickness)

              resulting_particles[[lon,lat+1]]$value      <- H_max - thickness
              resulting_particles[[lon,lat+1]]$slip_value <- resulting_particles[[lon,lat+1]]$slip_value - thickness
              resulting_particles[[lon,lat]]$value        <- resulting_particles[[lon,lat]]$value + thickness
              resulting_particles[[lon,lat]]$slip_value   <- ifelse(!isTRUE(is.na(resulting_particles[[lon,lat]]$slip_value)),resulting_particles[[lon,lat]]$value + thickness,thickness)
            }
          }
          else if (max(particles[[lon+1,lat]]$value,-1e10, na.rm = TRUE)==H_max){
            if(!isTRUE(is.na(particles[[lon+1,lat]]$slip_value))){
              particles[[lon+1,lat]]$value      <- H_max - thickness
              particles[[lon+1,lat]]$slip_value <- particles[[lon+1,lat]]$slip_value - thickness
              particles[[lon,lat]]$value        <- particles[[lon,lat]]$value + thickness
              particles[[lon,lat]]$slip_value   <- ifelse(!isTRUE(is.na(particles[[lon,lat]]$slip_value)),resulting_particles[[lon,lat]]$value + thickness,thickness)

              resulting_particles[[lon+1,lat]]$value      <- H_max - thickness
              resulting_particles[[lon+1,lat]]$slip_value <- resulting_particles[[lon+1,lat]]$slip_value - thickness
              resulting_particles[[lon,lat]]$value        <- resulting_particles[[lon,lat]]$value + thickness
              resulting_particles[[lon,lat]]$slip_value   <- ifelse(!isTRUE(is.na(resulting_particles[[lon,lat]]$slip_value)),resulting_particles[[lon,lat]]$value + thickness,thickness)
            }
          }
          else if (max(particles[[lon-1,lat]]$value,-1e10, na.rm = TRUE)==H_max){
            if(!isTRUE(is.na(particles[[lon-1,lat]]$slip_value))){
              particles[[lon-1,lat]]$value      <- H_max - thickness
              particles[[lon-1,lat]]$slip_value <- particles[[lon-1,lat]]$slip_value - thickness
              particles[[lon,lat]]$value        <- particles[[lon,lat]]$value + thickness
              particles[[lon,lat]]$slip_value   <- ifelse(!isTRUE(is.na(particles[[lon,lat]]$slip_value)),resulting_particles[[lon,lat]]$value + thickness,thickness)

              resulting_particles[[lon-1,lat]]$value      <- H_max - thickness
              resulting_particles[[lon-1,lat]]$slip_value <- resulting_particles[[lon-1,lat]]$slip_value - thickness
              resulting_particles[[lon,lat]]$value        <- resulting_particles[[lon,lat]]$value + thickness
              resulting_particles[[lon,lat]]$slip_value   <- ifelse(!isTRUE(is.na(resulting_particles[[lon,lat]]$slip_value)),resulting_particles[[lon,lat]]$value + thickness,thickness)
            }
          }
          else if (max(particles[[lon-1,lat-1]]$value,-1e10, na.rm = TRUE)==H_max){
            if(!isTRUE(is.na(particles[[lon-1,lat-1]]$slip_value))){
              particles[[lon-1,lat-1]]$value      <- H_max - thickness
              particles[[lon-1,lat-1]]$slip_value <- particles[[lon-1,lat-1]]$slip_value - thickness
              particles[[lon,lat]]$value        <- particles[[lon,lat]]$value + thickness
              particles[[lon,lat]]$slip_value   <- ifelse(!isTRUE(is.na(particles[[lon,lat]]$slip_value)),resulting_particles[[lon,lat]]$value + thickness,thickness)

              resulting_particles[[lon-1,lat-1]]$value      <- H_max - thickness
              resulting_particles[[lon-1,lat-1]]$slip_value <- resulting_particles[[lon-1,lat-1]]$slip_value - thickness
              resulting_particles[[lon,lat]]$value        <- resulting_particles[[lon,lat]]$value + thickness
              resulting_particles[[lon,lat]]$slip_value   <- ifelse(!isTRUE(is.na(resulting_particles[[lon,lat]]$slip_value)),resulting_particles[[lon,lat]]$value + thickness,thickness)
            }
          }
          else if (max(particles[[lon-1,lat+1]]$value,-1e10, na.rm = TRUE)==H_max){
            if(!isTRUE(is.na(particles[[lon-1,lat+1]]$slip_value))){
              particles[[lon-1,lat+1]]$value      <- H_max - thickness
              particles[[lon-1,lat+1]]$slip_value <- particles[[lon-1,lat]]$slip_value - thickness
              particles[[lon,lat]]$value        <- particles[[lon,lat]]$value + thickness
              particles[[lon,lat]]$slip_value   <- ifelse(!isTRUE(is.na(particles[[lon,lat]]$slip_value)),resulting_particles[[lon,lat]]$value + thickness,thickness)

              resulting_particles[[lon-1,lat+1]]$value      <- H_max - thickness
              resulting_particles[[lon-1,lat+1]]$slip_value <- resulting_particles[[lon-1,lat+1]]$slip_value - thickness
              resulting_particles[[lon,lat]]$value        <- resulting_particles[[lon,lat]]$value + thickness
              resulting_particles[[lon,lat]]$slip_value   <- ifelse(!isTRUE(is.na(resulting_particles[[lon,lat]]$slip_value)),resulting_particles[[lon,lat]]$value + thickness,thickness)
            }
          }
          else if (max(particles[[lon+1,lat+1]]$value,-1e10, na.rm = TRUE)==H_max){
            if(!isTRUE(is.na(particles[[lon+1,lat+1]]$slip_value))){
              particles[[lon+1,lat+1]]$value      <- H_max - thickness
              particles[[lon+1,lat+1]]$slip_value <- particles[[lon+1,lat+1]]$slip_value - thickness
              particles[[lon,lat]]$value        <- particles[[lon,lat]]$value + thickness
              particles[[lon,lat]]$slip_value   <- ifelse(!isTRUE(is.na(particles[[lon,lat]]$slip_value)),resulting_particles[[lon,lat]]$value + thickness,thickness)

              resulting_particles[[lon+1,lat+1]]$value      <- H_max - thickness
              resulting_particles[[lon+1,lat+1]]$slip_value <- resulting_particles[[lon+1,lat+1]]$slip_value - thickness
              resulting_particles[[lon,lat]]$value        <- resulting_particles[[lon,lat]]$value + thickness
              resulting_particles[[lon,lat]]$slip_value   <- ifelse(!isTRUE(is.na(resulting_particles[[lon,lat]]$slip_value)),resulting_particles[[lon,lat]]$value + thickness,thickness)
            }
          }
          else if (max(particles[[lon+1,lat-1]]$value,-1e10, na.rm = TRUE)==H_max){
            if(!isTRUE(is.na(particles[[lon+1,lat-1]]$slip_value))){
              particles[[lon+1,lat-1]]$value      <- H_max - thickness
              particles[[lon+1,lat-1]]$slip_value <- particles[[lon+1,lat+1]]$slip_value - thickness
              particles[[lon,lat]]$value        <- particles[[lon,lat]]$value + thickness
              particles[[lon,lat]]$slip_value   <- ifelse(!isTRUE(is.na(particles[[lon,lat]]$slip_value)),resulting_particles[[lon,lat]]$value + thickness,thickness)

              resulting_particles[[lon+1,lat-1]]$value      <- H_max - thickness
              resulting_particles[[lon+1,lat-1]]$slip_value <- resulting_particles[[lon+1,lat-1]]$slip_value - thickness
              resulting_particles[[lon,lat]]$value        <- resulting_particles[[lon,lat]]$value + thickness
              resulting_particles[[lon,lat]]$slip_value   <- ifelse(!isTRUE(is.na(resulting_particles[[lon,lat]]$slip_value)),resulting_particles[[lon,lat]]$value + thickness,thickness)
            }
          }




        }
      }



    }
  }

  return(resulting_particles)
}
