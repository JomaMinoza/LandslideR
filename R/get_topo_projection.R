#' get_topo_projection - Get the Raster Projection of the Input Topography
#'
#' @param topo
#' @param crs
#' @param resolution
#' @param ...
#'
#' @return raster
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
}
