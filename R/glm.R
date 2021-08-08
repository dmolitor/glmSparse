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
#' @param ... Potentially arguments passed on to fitter functions; currently
#'   unused.
#' @return An object of class `glpModel`.
#' @export
#' @seealso [Matrix::sparse.model.matrix()] for information on creating sparse
#'   model matrices, and [MatrixModels::glm4()] to understand more details on
#'   the implementation.
glmSparse <- function (formula,
                       family,
                       data,
                       weights,
                       subset,
                       na.action,
                       start = NULL,
                       etastart,
                       mustart,
                       offset,
                       doFit = TRUE,
                       control = list(...),
                       model = TRUE,
                       contrasts = NULL,
                       x,
                       y,
                       ...) {
  if (missing(formula) && missing(data) && (missing(x) || missing(y))) {
    stop("If no formula is supplied, both `x` and `y` must be supplied", call. = FALSE)
  }
  sparse <- TRUE
  drop.unused.levels <- TRUE
  call <- match.call()
  # If x and y are provided and formula is empty, then use the provided x & y
  if (!missing(x) && !missing(y) && missing(formula)) {
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
  if (missing(family)) {
    family <- NULL
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
  if (!missing(x) && !missing(y) && missing(formula)) {
    if (missing(weights)) weights <- NULL
    if (missing(offset)) offset <- NULL
    model_resp <- as_respMod(x = x,
                             y = y,
                             weights = weights,
                             offset = offset,
                             family = family)
    model_pred <- as_predModule(dgC_to_dsparseModel(x))
  } else {
    if (missing(data)) {
      stop(paste0("When `formula` is provided, `data` should never be missing.",
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
    model_pred <- as(
      MatrixModels::model.Matrix(formula,
                                 mf,
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
