#' get_resolution - Get the Raster Resolution of the Input Topography
#'
#' @param topo
#'
#' @return res
#' @export
#'
#' @examples
get_resolution <- function(topo){
  resolution <- res(as.raster(topo)) * 111111
  return(resolution)
}
