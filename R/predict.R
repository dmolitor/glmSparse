# Check matrix names
check_names <- function(original, new) {
  if (!identical(character(0), setdiff(original, new))) {
    stop("Column names in `newdata` must match original column names", call. = FALSE)
  }
  invisible(TRUE)
}

# Corollary to base predict.lm
predict_lm <- function(object,
                       newdata,
                       se.fit = FALSE,
                       interval = "none",
                       type = "response",
                       weights = 1,
                       ...) {
  tt <- terms(as.formula(object@call$formula))
  if (se.fit) {
    stop("`se.fit` == TRUE is not currently supported, sorry :(", call. = FALSE)
  }
  if (interval != "none") {
    stop("Currently only `interval` = 'none' is supported, sorry :(", call. = FALSE)
  }
  if (type != "response") {
    stop("Currently only `type` = 'response' is supported, sorry :(", call. = FALSE)
  }
  if (is.null(newdata)) {
    X <- pred_matrix(object)
    offset <- object@resp@offset
  } else {
    tt <- delete.response(tt)
    model_formula <- reconstruct_formula(object)
    if (!is.null(model_formula)) {
      stopifnot(length(model_formula) == 3)
      varnames <- setdiff(colnames(newdata), deparse(model_formula[[2]]))
      stopifnot(!identical(varnames, character(0)))
    } else {
      varnames <- NULL
    }
    X <- pred_matrix(object, newdata, contrasts.arg = object@pred@X@contrasts)
    if (is.null(varnames)) {
      varnames <- colnames(X)
      stopifnot(!is.null(varnames))
    }
    offset <- rep(0, nrow(X))
    if (!is.null(off.num <- attr(tt, "offset")))
      for (i in off.num) {
        offset <- offset + newdata[, attr(tt, "variables")[[i + 1]]]
      }
    if (!is.null(object@call$offset))  {
      offset <- offset + eval(object@call$offset)
    }
  }
  df.null <- object@pred@X@Dim[[1L]] - 1L
  df.residual <- df.null - (object@pred@X@Dim[[2L]] - 1L)
  n <- length(residuals(object))
  p <- as.vector(Matrix::rankMatrix(object@pred@X, method = "qr.R"))
  p1 <- seq_len(p)
  piv <- if (p) Matrix::expand(object@pred@fac)$P@perm[p1]
  if (p < ncol(X) && !is.null(newdata)) {
    warning("prediction from a rank-deficient fit may be misleading", call. = FALSE)
  }
  beta <- coef(object)
  predictor <- drop(Matrix::tcrossprod(X[, piv, drop = FALSE], beta[piv]))
  if (!identical(offset, rep(0, nrow(X)))) {
    predictor <- predictor + offset
  }
  predictor
}

#' @method predict glpModel
#' @export
predict.glpModel <- predict.glpModel <- function(object,
                                                 newdata = NULL,
                                                 type = c("link", "response"),
                                                 se.fit = FALSE,
                                                 dispersion = NULL,
                                                 ...) {
  type <- match.arg(type)
  if (!se.fit) {
    if (is.null(newdata)) {
      pred <- switch(
        type,
        link = object@resp@eta,
        response = object@resp@mu
      )
    } else {
      pred <- predict_lm(
        object,
        newdata,
        se.fit,
        type = if (type == "link") "response" else type
      )
      switch(
        type,
        response = {
          pred <- object@resp@family$linkinv(pred)
        },
        link = ,
      )
    }
  } else {
    if (is.null(dispersion) || dispersion == 0) {
      dispersion <- summary(object, dispersion = dispersion)$dispersion
    }
    residual.scale <- as.vector(sqrt(dispersion))
    pred <- predict_lm(
      object,
      newdata,
      se.fit,
      scale = residual.scale,
      type = if (type == "link") "response" else type
    )
    fit <- pred$fit
    se.fit <- pred$se.fit
    switch(
      type,
      response = {
        se.fit <- se.fit * abs(object@resp@family$mu.eta(fit))
        fit <- object@resp@family$linkinv(fit)
      },
      link =
    )
    pred <- list(fit = fit, se.fit = se.fit, residual.scale = residual.scale)
  }
  pred
}

# Create prediction matrix
pred_matrix <- function(object, newdata = NULL, ...) {
  model_formula <- reconstruct_formula(object, keep.lhs = FALSE)
  if (is.null(newdata)) {
    return(as(object@pred@X, "CsparseMatrix"))
  } else if (is.null(model_formula)) {
    check_names(object@pred@X@Dimnames[[2]], colnames(newdata))
    pred_mat <- newdata
  } else {
    pred_mat <- Matrix::sparse.model.matrix(
      object = stats::as.formula(model_formula),
      data = newdata,
      ...
    )
    check_names(object@pred@X@Dimnames[[2]], colnames(pred_mat))
  }
  pred_mat
}

# Reconstruct formula
reconstruct_formula <- function(object, keep.lhs = TRUE) {
  model_call <- object@call
  if (!is.null(model_call$data)) {
    if (length(model_call$formula) > 2 && !keep.lhs) {
      return(model_call$formula[-2])
    }
    return(model_call$formula)
  }
  NULL
}

