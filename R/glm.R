#' Fitting sparse Generalized Linear Models
#'
#' `glmSparse` is a thin wrapper around `MatrixModels::glm4`. The notable
#' differences are that `glmSparse` always fits using a sparse model matrix,
#' and that it provides an alternate interface for situations where the user
#' wants to directly provide a sparse model matrix and response variable instead
#' of using the more standard formula interface. For more details on the
#' underlying implementation, see `MatrixModels::glm4`.
#'
#' @param formula An object of class \link{formula}, (or one that can be
#'   coerced to that class): a symbolic description of the model to be fitted.
#' @param family A description of the error distribution and link function to be
#'   used in the model. This can be a character string naming a family function,
#'   a family function or the result of a call to a family function. (See
#'   \link{family} for details of family functions.)
#' @param data An optional data frame, list or environment (or object coercible
#'   by \link{as.data.frame} to a data frame) containing the variables in the
#'   model.
#' @param weights an optional vector of ‘prior weights’ to be used in the
#'   fitting process. Should be NULL or a numeric vector.
#' @param subset An optional vector specifying a subset of observations to be
#'   used in the fitting process.
#' @param na.action A function which indicates what should happen when the data
#'   contain `NA`s. The default is set by the na.action setting of \link{options},
#'   and is \link{na.fail} if that is unset. The ‘factory-fresh’ default is
#'   \link{na.omit}. Another possible value is `NULL`, no action. Value
#'   \link{na.exclude} can be useful.
#' @param start Starting values for the parameters in the linear predictor.
#' @param etastart Starting values for the parameters in the predictor itself.
#' @param mustart Starting values for the parameters in the vector of means.
#' @param offset This can be used to specify an \emph{a priori} known component
#'   to be included in the linear predictor during fitting. This should be `NULL`
#'   or a numeric vector of length equal to the number of cases. One or more
#'   offset terms can be included in the formula instead or as well, and if more
#'   than one is specified, their sum is used. See \link{model.offset}.
#' @param doFit Logical indicating if the model should be fitted
#'   (or just returned unfitted).
#' @param control A list with options on fitting; currently passed unchanged to
#'   (hidden) function `IRLS()`.
#' @param model Currently ignored; here for back-compatibility with \link{glm}.
#' @param contrasts Currently ignored; here for back-compatibility with \link{glm}.
#' @param x Alternate interface for modeling. An already-constructed sparse
#'   model matrix (class 'dgCMatrix').
#' @param y Alternate interface for modeling. A numeric response vector.
#' @param drop.unused.levels Should factors have unused levels dropped? Defaults
#'   to `FALSE`.
#' @param ... Potentially arguments passed on to fitter functions; currently
#'   unused.
#' @return An object of class `glpModel`.
#' @export
#' @seealso [Matrix::sparse.model.matrix()] for information on creating sparse
#'   model matrices, and [MatrixModels::glm4()] to understand more details on
#'   the implementation.
glmSparse <- function(formula = NULL,
                      family = stats::gaussian,
                      data = NULL,
                      weights = NULL,
                      subset = NULL,
                      na.action = stats::na.fail,
                      start = NULL,
                      etastart = NULL,
                      mustart = NULL,
                      offset = NULL,
                      doFit = TRUE,
                      control = list(...),
                      model = TRUE,
                      contrasts = NULL,
                      x = NULL,
                      y = NULL,
                      drop.unused.levels = FALSE,
                      ...) {
  if (is.null(formula) && is.null(data) && (is.null(x) || is.null(y))) {
    stop("If no formula is supplied, both `x` and `y` must be supplied", call. = FALSE)
  }
  if (is.null(na.action)) na.action <- stats::na.fail
  if (!identical(na.action, stats::na.fail)) {
    stop("`na.action` must be `stats::na.fail` - deal with NAs earlier ;)", call. = FALSE)
  }
  if (!is.null(offset) && !is.null(offset) && is.numeric(offset)) {
    offset <- as.numeric(offset)
  }
  if (is.null(family)) {
    family  <- stats::gaussian
  }
  sparse <- TRUE
  call <- match.call()
  # If x and y are provided and formula is empty, then use the provided x & y
  if (!is.null(x) && !is.null(y) && is.null(formula)) {
    if (!inherits(x, "dgCMatrix")) {
      stop(paste0("\nCurrently `x` is only supported as a sparse matrix.\nTo create",
                  " a sparse model matrix, see `Matrix::sparse.model.matrix`.\n",
                  "To convert a dense model matrix into sparse form, execute",
                  " `as(x, 'dgCMatrix')`."),
           call. = FALSE)
    }
    if (!is.null(dim(y)) || !is.numeric(y)) {
      stop("`y` must be a numeric vector, not an array or matrix.",
           call. = FALSE)
    }
    if (NROW(y) != nrow(x)) {
      stop("`y` and `x` have differing lengths.", call. = FALSE)
    }
    call$formula <- y ~ x
    call$data <- NULL
    call$x <- NULL
    call$y <- NULL
  }
  if (is.null(family)) {
    stop("`family` must be specified", call. = FALSE)
  } else {
    if (is.character(family)) {
      family <- get(family, mode = "function", envir = parent.frame())
    }
    if (is.function(family)) {
      family <- family()
    }
    if (is.null(family$family)) {
      print(family)
      stop("'family' not recognized")
    }
  }
  if (!is.null(x) && !is.null(y) && is.null(formula)) {
    if (is.null(weights)) weights <- NULL
    if (is.null(offset)) offset <- NULL
    model_resp <- as_respMod(x = x,
                             y = y,
                             weights = weights,
                             offset = offset,
                             family = family)
    model_pred <- as_predModule(dgC_to_dsparseModel(x))
  } else {
    if (is.null(data)) {
      stop(paste0("When `formula` is provided, `data` should never be is.null.",
                  " To use an already-created model matrix and response variable,",
                  " provide these data in the `x` and `y` arguments, respectively."),
           call. = FALSE)
    }
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset",
                 "weights", "na.action", "etastart",
                 "mustart", "offset"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- as.name("model.frame")
    mf <- eval(mf, parent.frame())
    model_resp <- mkRespMod(mf, family)
    mt <- attr(mf, "terms")
    model_pred <- as(
      MatrixModels::model.Matrix(mt,
                                 mf,
                                 contrasts,
                                 sparse = sparse,
                                 drop.unused.levels = drop.unused.levels),
      "predModule"
    )
  }
  ans <- new("glpModel",
             call = call,
             resp = model_resp,
             pred = model_pred)
  if (doFit) {
    return(fit_glmSparse(ans, doFP = TRUE, control = control))
  } else {
    return(ans)
  }
}

#' @rdname show-methods
#' @aliases show,glpModel,ANY-method
setMethod(
  "show",
  "glpModel",
  function(object) {
    digits = max(3L, getOption("digits") - 3L)
    df.null <- object@pred@X@Dim[[1L]] - 1L
    df.residual <- df.null - (object@pred@X@Dim[[2L]] - 1L)
    rank <- as.vector(Matrix::rankMatrix(object@pred@X, method = "qr.R"))
    intercept <- any(object@pred@X@assign == 0)
    wtdmu <- if (intercept) {
      sum(object@resp@weights * object@resp@y)/sum(object@resp@weights)
    } else {
      object@resp@family$linkinv(object@resp@offset)
    }
    dev <- sum(
      object@resp@family$dev.resids(y = object@resp@y,
                               mu = object@resp@mu,
                               wt = object@resp@weights)
    )
    nulldev <- sum(
      object@resp@family$dev.resids(y = object@resp@y,
                               mu = wtdmu,
                               wt = object@resp@weights)
    )
    aic <- object@resp@family$aic(y = object@resp@y,
                             n = object@resp@n,
                             mu = object@resp@mu,
                             wt = object@resp@weights,
                             dev = dev) + 2 * rank
    cat(
      "\nCall:  ",
      paste(deparse(object@call),
            sep = "\n",
            collapse = "\n"),
      "\n\n",
      sep = ""
    )
    if (length(MatrixModels::coef(object))) {
      cat("Coefficients")
      if (is.character(co <- object@pred@X@contrasts))
        cat(
          "  [contrasts: ",
          apply(cbind(names(co), co), 1L, paste, collapse = "="),
          "]"
        )
      cat(":\n")
      print.default(
        format(MatrixModels::coef(object),
               digits = digits),
        print.gap = 2,
        quote = FALSE
      )
    } else {
      cat("No coefficients\n\n")
    }
    cat(
      "\nDegrees of Freedom:",
      df.null, "Total (i.e. Null); ",
      df.residual, "Residual\n"
    )
    cat(
      "Null Deviance:\t   ",
      format(signif(nulldev, digits)),
      "\nResidual Deviance:",
      format(signif(dev, digits)),
      "\tAIC:",
      format(signif(aic, digits))
    )
    cat("\n")
    invisible(object)
  }
)
