#' @method fitted glpModel
#' @export
fitted.glpModel <- function(object, ...) {
  MatrixModels::fitted(object, ...)
}

#' @method residuals glpModel
#' @export
residuals.glpModel <- function(object, ...) {
  resids <- MatrixModels::residuals(object, ...)
  names(resids) <- object@pred@X@Dimnames[[1]]
  resids
}

#' @method tidy glpModel
#' @export
tidy.glpModel <- function(x,
                          conf.int = FALSE,
                          conf.level = 0.95,
                          exponentiate = FALSE,
                          ...) {
  ret <- tibble::as_tibble(summary(x)$coefficients, rownames = "term")
  colnames(ret) <- c("term", "estimate", "std.error",
                     "statistic", "p.value")
  coefs <- tibble::enframe(MatrixModels::coef(x),
                           name = "term",
                           value = "estimate")
  ret <- dplyr::left_join(coefs, ret, by = c("term", "estimate"))
  if (conf.int) {
    ci <- confint_terms(x, level = conf.level)
    ret <- dplyr::left_join(ret, ci, by = "term")
  }
  if (exponentiate) {
    ret <- exponentiate(ret)
  }
  ret
}

#' @importFrom broom tidy
#' @export
broom::tidy
