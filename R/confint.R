#' @method confint glpModel
#' @export
confint.glpModel <- function (object, parm, level = 0.95, ...) {
  cf <- MatrixModels::coef(object)
  pnames <- names(cf)
  if (missing(parm))
    parm <- pnames
  else if (is.numeric(parm))
    parm <- pnames[parm]
  a <- (1 - level)/2
  a <- c(a, 1 - a)
  pct <- paste(format(100 * a, trim = TRUE, scientific = FALSE, digits = 3), "%")
  fac <- stats::qnorm(a)
  ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm, pct))
  ses <- sqrt(diag(stats::vcov(object)))[parm]
  ci[] <- cf[parm] + outer(ses, fac)
  ci
}

# This function is an, slightly modified, unexported function from broom, namely
# `broom:::broom_confint_terms()`.
confint_terms <- function(x, ...) {
  ci <- suppressMessages(confint.glpModel(x, ...))
  if (is.null(dim(ci))) {
    ci <- matrix(ci, nrow = 1)
    rownames(ci) <- names(MatrixModels::coef(x))[1]
  }
  ci <- tibble::as_tibble(ci, rownames = "term")
  names(ci) <- c("term", "conf.low", "conf.high")
  ci
}
