#' replaceNA - Replace NA with Zeroes
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
replaceNA      <- function(x) { replace(x, is.na(x), 0) }
