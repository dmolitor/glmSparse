#' @method print summary.glpModel
#' @export
print.summary.glpModel <- function(x,
                                   digits = max(3L, getOption("digits") - 3L),
                                   symbolic.cor = x$symbolic.cor,
                                   signif.stars = getOption("show.signif.stars"),
                                   ...) {
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n",
                         collapse = "\n"), "\n\n", sep = "")
  cat("Deviance Residuals: \n")
  if (x$df.residual > 5) {
    x$deviance.resid <- stats::setNames(
      stats::quantile(x$deviance.resid, na.rm = TRUE),
      c("Min", "1Q", "Median", "3Q", "Max")
    )
  }
  xx <- zapsmall(x$deviance.resid, digits + 1L)
  print.default(xx, digits = digits, na.print = "", print.gap = 2L)
  if (length(x$aliased) == 0L) {
    cat("\nNo Coefficients\n")
  } else {
    df <- if ("df" %in% names(x)) x[["df"]] else NULL
    if (!is.null(df) && (nsingular <- df[3L] - df[1L])) {
      cat("\nCoefficients: (",
          nsingular,
          " not defined because of singularities)\n",
          sep = "")
    } else {
      cat("\nCoefficients:\n")
    }
    coefs <- x$coefficients
    if (!is.null(aliased <- x$aliased) && any(aliased)) {
      cn <- names(aliased)
      coefs <- matrix(NA,
                      length(aliased),
                      4L,
                      dimnames = list(cn, colnames(coefs)))
      coefs[!aliased, ] <- x$coefficients
    }
    stats::printCoefmat(coefs,
                        digits = digits,
                        signif.stars = signif.stars,
                        na.print = "NA",
                        ...)
  }
  cat("\n(Dispersion parameter for ", x$family$family,
      " family taken to be ", format(x$dispersion), ")\n\n",
      apply(cbind(paste(format(c("Null", "Residual"),
                               justify = "right"), "deviance:"),
                  format(unlist(x[c("null.deviance",
                                    "deviance")]), digits = max(5L, digits + 1L)),
                  " on", format(unlist(x[c("df.null", "df.residual")])),
                  " degrees of freedom\n"), 1L, paste, collapse = " "),
      sep = "")
  cat("AIC: ", format(x$aic, digits = max(4L, digits + 1L)),
      "\n\n", "Number of Fisher Scoring iterations: ",
      x$iter, "\n", sep = "")
  correl <- x$correlation
  if (!is.null(correl)) {
    p <- NCOL(correl)
    if (p > 1) {
      cat("\nCorrelation of Coefficients:\n")
      if (is.logical(symbolic.cor) && symbolic.cor) {
        print(stats::symnum(correl, abbr.colnames = NULL))
      } else {
        correl <- format(round(correl, 2L), nsmall = 2L,
                         digits = digits)
        correl[!lower.tri(correl)] <- ""
        print(correl[-1, -p, drop = FALSE], quote = FALSE)
      }
    }
  }
  cat("\n")
  invisible(x)
}

#' @method summary glpModel
#' @export
summary.glpModel <- function(object,
                             dispersion = NULL,
                             correlation = FALSE,
                             symbolic.cor = FALSE,
                             ...) {
  ### Calculate attributes that don't yet exist ###
  intercept <- any(object@pred@X@assign == 0)
  df.null <- object@pred@X@Dim[[1L]] - 1L
  df.residual <- df.null - (object@pred@X@Dim[[2L]] - 1L)
  rank <- as.vector(Matrix::rankMatrix(object@pred@X, method = "qr.R"))
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
  #################################################
  est.disp <- FALSE
  df.r <- df.residual
  if (is.null(dispersion)) {
    dispersion <- if (object@resp@family[[1]] %in% c("poisson", "binomial")) {
      1
    } else if (df.r > 0) {
      est.disp <- TRUE
      if (any(object@resp@weights == 0))
        warning("observations with zero weight not used for calculating dispersion")
      sum((object@resp@weights * object@resp@wtres^2)[object@resp@weights > 0])/df.r
    } else {
      est.disp <- TRUE
      NaN
    }
  }
  aliased <- is.na(MatrixModels::coef(object))
  p <- rank
  if (p > 0) {
    p1 <- 1L:p
    Qr <- object@pred@fac
    coef.p <- MatrixModels::coef(object)[sort(Matrix::expand(Qr)$P@perm[p1])]
    covmat.unscaled <- Matrix::chol2inv(Qr)
    dimnames(covmat.unscaled) <- list(names(coef.p), names(coef.p))
    covmat <- dispersion * covmat.unscaled
    var.cf <- Matrix::diag(covmat)
    s.err <- sqrt(var.cf)
    tvalue <- coef.p/s.err
    dn <- c("Estimate", "Std. Error")
    if (!est.disp) {
      pvalue <- 2 * stats::pnorm(-abs(tvalue))
      coef.table <- cbind(coef.p, s.err, tvalue, pvalue)
      dimnames(coef.table) <- list(names(coef.p), c(dn, "z value", "Pr(>|z|)"))
    } else if (df.r > 0) {
      pvalue <- 2 * stats::pt(-abs(tvalue), df.r)
      coef.table <- cbind(coef.p, s.err, tvalue, pvalue)
      dimnames(coef.table) <- list(names(coef.p), c(dn, "t value", "Pr(>|t|)"))
    } else {
      coef.table <- cbind(coef.p, NaN, NaN, NaN)
      dimnames(coef.table) <- list(names(coef.p), c(dn, "t value", "Pr(>|t|)"))
    }
    df.f <- Qr@Dim[[2]]
  } else {
    coef.table <- matrix(, 0L, 4L)
    dimnames(coef.table) <- list(NULL, c("Estimate",
                                         "Std. Error", "t value", "Pr(>|t|)"))
    covmat.unscaled <- covmat <- matrix(, 0L, 0L)
    df.f <- length(aliased)
  }
  keep <- list("call" = object@call,
               "family" = object@resp@family,
               "link" = object@resp@family$link,
               "deviance" = dev,
               "aic" = aic,
               "contrasts" = unlist(object@pred@X@contrasts),
               "df.residual" = df.residual,
               "null.deviance" = nulldev,
               "df.null" = df.null,
               "iter" = object@fitProps$iter)
  ans <- c(
    keep,
    list(deviance.resid = MatrixModels::residuals(object, type = "deviance"),
         coefficients = coef.table,
         aliased = aliased,
         dispersion = dispersion,
         df = c(rank, df.r, df.f),
         cov.unscaled = as.matrix(covmat.unscaled),
         cov.scaled = as.matrix(covmat))
  )
  if (correlation && p > 0) {
    dd <- sqrt(Matrix::diag(covmat.unscaled))
    ans$correlation <- as.matrix(covmat.unscaled/outer(dd, dd))
    ans$symbolic.cor <- symbolic.cor
  }
  class(ans) <- "summary.glpModel"
  return(ans)
}
