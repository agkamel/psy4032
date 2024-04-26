#' Compute population variance and standard deviation
#'
#' @param x Numeric vector.
#'
#' @return Numeric scalar.
#'
#' @examples
#' x <- rnorm(10)
#' var_pop(x)
#' sd_pop(x)
#' @rdname var_sd_pop
#' @export
var_pop <- function(x) {
  stopifnot("x must be numeric" = is.numeric(x))
  sum((x - mean(x))^2) / length(x)
}


#' @rdname var_sd_pop
#' @export
sd_pop <- function(x) {
  sqrt(var_pop(x))
}
