#' @method vcov glpModel
#' @export
vcov.glpModel <- function (object, complete = TRUE, ...) {
  vcov.summary.glpModel(summary.glpModel(object, ...), complete = complete)
}

#' @method vcov summary.glpModel
#' @export
vcov.summary.glpModel <- function (object, complete = TRUE, ...) {
  stats::.vcov.aliased(object$aliased, object$cov.scaled, complete = complete)
}
