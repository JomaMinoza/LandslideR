#' replaceZero - Replace Zeroes with NA
#'
#' @param x A numeric vector or matrix
#'
#' @return A numeric vector or matrix with zeros replaced by NA
#' @export
#'
#' @examples
#' replaceZero(c(1, 0, 3, 0, 5))
#'
replaceZero     <- function(x) { replace(x, x == 0, NA) }
