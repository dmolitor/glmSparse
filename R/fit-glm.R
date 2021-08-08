#' @importFrom methods as is new
#' @import Matrix MatrixModels

# Export MatrixModels:::fitGlm4
fit_glmSparse <- function (lp, doFP = TRUE, control = list()) {
  if (doFP && is(lp@resp, "glmRespMod"))
    lp@pred@coef <- glm.fp(lp)
  IRLS(lp, control)
}

# Export MatrixModels:::glm.fp
glm.fp <- function (lp) {
  stopifnot(is(lp, "glpModel"), is(rM <- lp@resp, "glmRespMod"))
  ff <- rM@family
  mu <- rM@mu
  vv <- ff$variance(mu)
  eta <- rM@eta
  muEta <- ff$mu.eta(eta)
  wts <- rM@weights
  z <- (eta - rM@offset) + (rM@y - rM@mu)/muEta
  good <- is.finite(vv) & vv > 0 & is.finite(z)
  stopifnot(any(good))
  w <- sqrt(wts * muEta * muEta/vv)[good]
  wM <- lp@pred@X[good, ] * w
  as.vector(solve(Matrix::crossprod(wM), Matrix::crossprod(wM, z[good] * w)))
}

# Export MatrixModels:::IRLS
IRLS <- function(mod, control) {
  stopifnot(is(mod, "glpModel"))
  respMod <- mod@resp
  predMod <- mod@pred
  MXITER <- warnOnly <- verbose <- quick <- TOL <- SMIN <- finalUpdate <- NULL
  do.defaults(control,
              list(MXITER = 200L, TOL = 0.0001, SMIN = 0.0001,
                   verbose = 0L, warnOnly = FALSE, quick = TRUE, finalUpdate = FALSE),
              environment())
  cc <- predMod@coef
  respMod <- MatrixModels::updateMu(respMod, as.vector(predMod@X %*% cc))
  iter <- nHalvings <- 0
  DONE <- FALSE
  repeat {
    if ((iter <- iter + 1) > MXITER) {
      msg <- paste("Number of iterations exceeded maximum MXITER =",
                   MXITER)
      if (!warnOnly)
        stop(msg)
      warning(msg)
      cc <- cbase
      DONE <- TRUE
      break
    }
    cbase <- cc
    respMod <- MatrixModels::updateWts(respMod)
    wrss0 <- sum(respMod@wtres^2)
    predMod <- MatrixModels::reweightPred(predMod, respMod@sqrtXwt, respMod@wtres)
    incr <- MatrixModels::solveCoef(predMod)
    convcrit <- sqrt(attr(incr, "sqrLen")/wrss0)
    if (verbose)
      cat(sprintf("_%d_ convergence criterion: %5g\n",
                  iter, convcrit))
    if (quick)
      if (convcrit < TOL)
        break
    step <- 1
    repeat {
      cc <- as.vector(cbase + step * incr)
      respMod <- MatrixModels::updateMu(respMod, as.vector(predMod@X %*% cc))
      wrss1 <- sum(respMod@wtres^2)
      if (verbose) {
        cat(sprintf("step = %.5f, new wrss = %.8g, Delta(wrss)= %g, coef =\n",
                    step, wrss1, wrss0 - wrss1))
        print(cc)
      }
      if (wrss1 < wrss0)
        break
      if ((step <- step/2) < SMIN) {
        msg <- "Minimum step factor 'SMIN' failed to reduce wrss"
        if (!warnOnly)
          stop(msg)
        warning(msg)
        cc <- cbase
        DONE <- TRUE
        break
      }
      if (DONE <- convcrit < TOL)
        break
      nHalvings <- nHalvings + 1
    }
    if (DONE || (!quick && convcrit < TOL))
      break
  }
  predMod@coef <- cc
  if (finalUpdate) {
    respMod <- MatrixModels::updateWts(respMod)
    predMod <- MatrixModels::reweightPred(predMod, respMod@sqrtXwt, respMod@wtres)
  }
  mod@fitProps <- list(convcrit = convcrit, iter = iter, nHalvings = nHalvings)
  mod@resp <- respMod
  mod@pred <- predMod
  mod
}
