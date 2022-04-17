#' replaceZero - Replace Zeroes with NA
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#'
replaceZero     <- function(x) { replace(x, isZero(x), NA) }
