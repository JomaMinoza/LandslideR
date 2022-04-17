#' get_resampled_topo - Get the Resampled of the Input Topography
#'
#' @param topo
#' @param crs
#' @param resolution
#' @param ...
#'
#' @return bathy
#' @export
#'
#' @examples
get_resampled_topo <- function(topo, crs, resolution = c(0.0041656,0.0041656), ...){

  projection <- get_topo_projection(topo = topo, crs = crs, resolution = resolution, ...)

  resampled_topo <- resample(
    as.raster(topo),
    projection,
    method = "ngb",
    ...
  )

  return(resampled_topo)
}
