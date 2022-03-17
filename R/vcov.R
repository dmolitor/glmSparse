#' @method vcov glpModel
#' @export
vcov.glpModel <- function (object, complete = TRUE, ...) {
  stats::vcov(summary(object, ...), complete = complete)
}

#' @method vcov summary.glpModel
#' @export
vcov.summary.glpModel <- function (object, complete = TRUE, ...) {
  stats::.vcov.aliased(object$aliased, object$cov.scaled, complete = complete)
}
