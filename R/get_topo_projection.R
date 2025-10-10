#' get_topo_projection - Get the Raster Projection of the Input Topography
#'
#' @param topo A bathymetric or topographic object
#' @param crs Coordinate reference system string
#' @param resolution Numeric vector of x,y resolution in degrees
#' @param ... Additional arguments passed to projectRaster
#'
#' @return A projected raster object
#' @export
#'
#' @examples
get_topo_projection <- function(topo, crs, resolution = c(0.0041656,0.0041656), ...){
  resampled_topo <- projectRaster(
    marmap::as.raster(topo),
    crs = crs,
    res = resolution,
    ...
  )
  return(resampled_topo)
}
