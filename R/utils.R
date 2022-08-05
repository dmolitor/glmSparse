#' @importFrom methods as is new slotNames
#' @import Matrix MatrixModels
#' @importFrom stats as.formula delete.response terms

# Convert sparse model matrix into class `sPredModule`
as_predModule <- function(x) {
  as(x, "predModule")
}

# Create RespMod object from existing sparse model matrix
as_respMod <- function(x,
                       y,
                       weights = NULL,
                       offset = NULL,
                       family = NULL,
                       nlenv = NULL,
                       nlmod = NULL) {
  stopifnot(nrow(x) == NROW(y))
  N <- n <- nrow(x)
  if (!is.null(nlmod)) {
    nleta <- eval(nlmod, nlenv)
    grad <- attr(nleta, "gradient")
    if (is.null(grad))
      stop("At present a nonlinear model must return a gradient attribute")
    N <- n * ncol(grad)
  }
  if (length(dim(y)) == 1) {
    nm <- rownames(y)
    dim(y) <- NULL
    if (!is.null(nm))
      names(y) <- nm
  }
  if (is.null(weights)) {
    weights <- rep.int(1, n)
  } else if (any(weights < 0)) {
    stop(gettext("negative weights not allowed", domain = "R-Matrix"))
  }
  if (is.null(offset)) {
    offset <- numeric(N)
  }
  if (length(offset) == 1) {
    offset <- rep.int(offset, N)
  } else if (length(offset) != N) {
    stop(gettextf("number of offsets (%d) should be %d (s * n)",
                  length(offset), N), domain = "R-Matrix")
  }
  ll <- list(weights = unname(weights), offset = unname(offset), wtres = numeric(n))
  if (!is.null(family)) {
    ll$y <- y
    rho <- new.env()
    rho$etastart <- NULL
    rho$mustart <- NULL
    rho$nobs <- n
    if (is.character(family)) {
      family <- get(family, mode = "function", envir = parent.frame(3))
    }
    if (is.function(family)) {
      family <- family()
    }
    eval(family$initialize, rho)
    family$initialize <- NULL
    ll$mu <- unname(rho$mustart)
    lr <- as.list(rho)
    ll[names(lr)] <- lr
    ll$weights <- unname(ll$weights)
    ll$y <- unname(ll$y)
    ll$eta <- family$linkfun(ll$mu)
    ll$sqrtrwt <- sqrt(ll$weights/family$variance(ll$mu))
    ll$sqrtXwt <- matrix(ll$sqrtrwt * family$mu.eta(ll$eta))
    ll$family <- family
    ll <- ll[intersect(names(ll), slotNames("glmRespMod"))]
    ll$n <- unname(rho$n)
    ll$Class <- "glmRespMod"
  }
  else {
    ll$sqrtrwt <- sqrt(ll$weights)
    ll$y <- unname(as.numeric(y))
    ll$mu <- numeric(n)
    if (is.null(nlenv)) {
      ll$Class <- "respModule"
      ll$sqrtXwt <- matrix(ll$sqrtrwt)
    }
    else {
      ll$Class <- "nlsRespMod"
      ll$nlenv <- nlenv
      ll$nlmod <- quote(nlmod)
      ll$sqrtXwt <- grad
      ll$pnames <- colnames(ll$sqrtXwt)
    }
  }
  do.call("new", ll)
}

# Convert from class `dgCMatrix` to class `dsparseModelMatrix`
dgC_to_dsparseModel <- function(x) {
  stopifnot(inherits(x, "dgCMatrix"))
  new("dsparseModelMatrix",
      x,
      assign = (1L:ncol(x)) - 1L,
      contrasts = if (is.null(ctr <- attr(x, "contrasts"))) list() else ctr)
}

# Export MatrixModels:::do.defaults
do.defaults <- function(control,
                        defaults,
                        rho,
                        nomatch.action = c("stop", "warning", "none")) {
  nomatch.action <- match.arg(nomatch.action)
  dnms <- names(defaults <- as.list(defaults))
  lapply(dnms, function(nm) assign(nm, defaults[[nm]], rho))
  matched <- !is.na(mm <- pmatch(names(control <- as.list(control)), dnms))
  if (nomatch.action != "none" && any(!matched)) {
    msg <- paste("The following control arguments did not match any default's names:",
                 paste(dQuote(names(control)[!matched]), collapse = ", "),
                 sep = "\n   ")
    switch(nomatch.action,
           warning = warning(msg, call. = FALSE,
                             immediate. = TRUE),
           stop = stop(msg, call. = FALSE))
  }
  if (any(matched)) {
    cc <- control[matched]
    names(cc) <- dnms[mm[matched]]
    lapply(names(cc), function(nm) assign(nm, as(cc[[nm]], class(defaults[[nm]])), rho))
  }
  invisible()
}

# Slightly modified `broom:::exponentiate()` function
#
# Uses updated dplyr semantics of `dplyr::across` within `dplyr::mutate`
# instead of `dplyr::mutate_at`
exponentiate <- function(data) {
  data <- dplyr::mutate(data, "estimate" = exp(eval(str2expression("estimate"))))
  if ("conf.low" %in% colnames(data)) {
    data <- dplyr::mutate(
      data,
      dplyr::across(.cols = c("conf.low", "conf.high"), .fns = exp)
    )
  }
  data
}

# Convert '('s in column names to '.' - make them syntactic!!
syntactic_names <- function(x) {
  cnames <- colnames(x)
  stopifnot(is.character(cnames), !identical(cnames, character(0)))
  cnames <- gsub("\\(|\\)", "\\.", cnames)
  colnames(x) <- cnames
  x
}
