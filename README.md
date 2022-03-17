
<!-- README.md is generated from README.Rmd. Please edit that file -->

# glmSparse

<!-- badges: start -->

[![R-CMD-check](https://github.com/dmolitor/glmSparse/workflows/R-CMD-check/badge.svg)](https://github.com/dmolitor/glmSparse/actions)
[![pkgdown](https://github.com/dmolitor/glmSparse/workflows/pkgdown/badge.svg)](https://github.com/dmolitor/glmSparse/actions)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of glmSparse is to provide common methods such as `coef`,
`summary`, `predict`, `broom::tidy`, etc. for sparse GLMs implemented in
the
[MatrixModels](https://cran.r-project.org/web/packages/MatrixModels/index.html)
package. For more information on this particular implementation of GLMs,
see the documentation for `MatrixModels::glm4`. Additionally, glmSparse
extends `MatrixModels::glm4` to allow the user to both fit GLMs using
the formula syntax of base R’s `glm` function as well as directly
passing in a pre-formulated sparse model matrix and response vector
(sparse matrices as defined by the `Matrix` package implementation).

## Motivation

The main motivation for this package is the times where I have wanted to
live my life peaceably and just run a huge (pretty sparse) OLS or logit
regression with no trouble, and I don’t have the memory on hand to run
the regression with a standard dense matrix. Unfortunately, I’ve
continued to be surprised by how little support there is to do this
within existing packages! (If I’m wrong about this, I would love to be
proved wrong). I discovered the potential solution to this problem with
`MatrixModels::glm4`, described as an experimental function way back in
like 2016, with very little support in the way of methods and nothing
seeming to change since that point. So, I took it upon myself to extend
this function’s functionality and make the methods that I wished it had.

## Alternatives

Here are the alternatives I’m aware of:

-   **glmnet::bigGlm**

    This works fine actually for estimating model parameters and making
    predictions, but because it’s not finding a closed-form solution it
    doesn’t out-of-the-box come with features that I would typically
    want (e.g. standard errors). It also doesn’t really have any of the
    standard methods you come to expect with `glm` such as `fitted`,
    `resid`, `summary`, etc.

-   **speedglm::speedglm**

    This option is confusing to me particularly because it has an
    argument `sparse` with the following description: “logical. Is the
    model matrix sparse? By default is NULL, so a quickly sample survey
    will be made.” However, the function does not allow the user to
    input sparse matrices from the `Matrix` package. Additionally, under
    the hood it is using `model.matrix` to create the underlying
    modeling matrix which is the exact step I’m trying to avoid (e.g. by
    using `Matrix::sparse.model.matrix`). It is possible that this
    function will allow sparse matrix classes from a different package
    (e.g. `SparseM`), but even if it does that’s not a great solution
    IMO because the `Matrix` implementation of sparse matrices are by
    far the most ubiquitous and I don’t really want to veer from that.

-   **glm**

    Not meant to support sparse matrices.

-   **Others**

    I’m potentially missing a good option here out of ignorance?

Assuming I’m not missing out on anything obvious here, none of the
options above provide a suitable solution to the issue, which is
presumably why the `MatrixModels::glm4` function was created in the
first place!

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("dmolitor/glmSparse")
```

## Example

In this example we’ll use the `dplyr::starwars` dataset which has
characteristics of a bunch of Star Wars characters, and we’ll run a
simple logistic regression to predict the characters’ genders. This will
showcase the alternate modeling interfaces as well as the standard
modeling methods.

First, we’ll do a tiny bit of data cleaning and we’ll also create a
sparse modeling matrix and response vector.

``` r
library(dplyr)
library(glmSparse)
library(Matrix)

# Prep the data for modeling
starwars_clean <- starwars |>
  select(height:eye_color, gender) |>
  filter(!is.na(gender)) |>
  mutate(
    gender = as.integer(factor(gender)) - 1,
    across(where(is.character), ~ factor(.x, exclude = NULL)),
    across(contains("color"), ~ forcats::fct_lump_min(.x, 5)),
    across(c(height, mass), ~ replace(.x, is.na(.x), median(.x, TRUE)))
  )

# Pre-create sparse modeling matrix
starwars_x <- sparse.model.matrix(gender ~ . - 1, starwars_clean)
starwars_y <- starwars_clean$gender
```

Now that we’ve created the modeling data let’s estimate a logistic
regression where `gender` is our response variable. We’ll use both the
formula and matrix interfaces.

``` r
## glmSparse

# Formula interface
logit_formula <- glmSparse(gender ~ ., family = binomial, data = starwars_clean)

# Matrix interface
logit_matrix <- glmSparse(x = starwars_x, y = starwars_y, family = binomial)
```

Let’s take a quick look at a few of the most helpful methods:

``` r
# Print the model
logit_formula
#> 
#> Call:  glmSparse(formula = gender ~ ., family = binomial, data = starwars_clean)
#> 
#> Coefficients:
#>     (Intercept)           height             mass  hair_colorbrown  
#>       -5.247662        -0.001529         0.063185         0.694687  
#>  hair_colornone     hair_colorNA  hair_colorOther   skin_colorfair  
#>        1.618806        14.583680         0.304483         0.181056  
#> skin_colorgreen   skin_colorgrey  skin_colorlight  skin_colorOther  
#>       17.714744         0.204344        -2.307464         0.306911  
#>   eye_colorblue   eye_colorbrown  eye_colororange     eye_colorred  
#>        0.845282         2.647640        17.520499        14.293584  
#> eye_coloryellow   eye_colorOther  
#>        2.007027         1.273756  
#> 
#> Degrees of Freedom: 82 Total (i.e. Null);  65 Residual
#> Null Deviance:       84.16 
#> Residual Deviance: 56.47     AIC: 92.47

# Get the summary of the model
summary(logit_formula)
#> 
#> Call:
#> glmSparse(formula = gender ~ ., family = binomial, data = starwars_clean)
#> 
#> Deviance Residuals: 
#>     Min       1Q   Median       3Q      Max  
#> -2.3899   0.0000   0.2656   0.6361   1.7203  
#> 
#> Coefficients:
#>                   Estimate Std. Error z value Pr(>|z|)  
#> (Intercept)     -5.248e+00  3.599e+00  -1.458   0.1448  
#> height          -1.529e-03  1.537e-02  -0.099   0.9208  
#> mass             6.319e-02  2.778e-02   2.275   0.0229 *
#> hair_colorbrown  6.947e-01  1.297e+00   0.536   0.5922  
#> hair_colornone   1.619e+00  1.348e+00   1.201   0.2296  
#> hair_colorNA     1.458e+01  3.187e+03   0.005   0.9963  
#> hair_colorOther  3.045e-01  1.442e+00   0.211   0.8327  
#> skin_colorfair   1.811e-01  1.784e+00   0.101   0.9192  
#> skin_colorgreen  1.771e+01  3.512e+03   0.005   0.9960  
#> skin_colorgrey   2.043e-01  2.111e+00   0.097   0.9229  
#> skin_colorlight -2.307e+00  1.749e+00  -1.319   0.1871  
#> skin_colorOther  3.069e-01  1.513e+00   0.203   0.8392  
#> eye_colorblue    8.453e-01  1.643e+00   0.514   0.6070  
#> eye_colorbrown   2.648e+00  1.919e+00   1.380   0.1676  
#> eye_colororange  1.752e+01  3.013e+03   0.006   0.9954  
#> eye_colorred     1.429e+01  3.505e+03   0.004   0.9967  
#> eye_coloryellow  2.007e+00  1.848e+00   1.086   0.2775  
#> eye_colorOther   1.274e+00  1.567e+00   0.813   0.4163  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 84.164  on 82  degrees of freedom
#> Residual deviance: 56.472  on 65  degrees of freedom
#> AIC: 92.472
#> 
#> Number of Fisher Scoring iterations: 17

# Tidy the summary of the model
tidy(logit_formula, exponentiate = TRUE)
#> # A tibble: 18 x 5
#>    term            estimate std.error statistic p.value
#>    <chr>              <dbl>     <dbl>     <dbl>   <dbl>
#>  1 (Intercept)      5.26e-3    3.60    -1.46     0.145 
#>  2 height           9.98e-1    0.0154  -0.0995   0.921 
#>  3 mass             1.07e+0    0.0278   2.27     0.0229
#>  4 hair_colorbrown  2.00e+0    1.30     0.536    0.592 
#>  5 hair_colornone   5.05e+0    1.35     1.20     0.230 
#>  6 hair_colorNA     2.16e+6 3187.       0.00458  0.996 
#>  7 hair_colorOther  1.36e+0    1.44     0.211    0.833 
#>  8 skin_colorfair   1.20e+0    1.78     0.101    0.919 
#>  9 skin_colorgreen  4.94e+7 3512.       0.00504  0.996 
#> 10 skin_colorgrey   1.23e+0    2.11     0.0968   0.923 
#> 11 skin_colorlight  9.95e-2    1.75    -1.32     0.187 
#> 12 skin_colorOther  1.36e+0    1.51     0.203    0.839 
#> 13 eye_colorblue    2.33e+0    1.64     0.514    0.607 
#> 14 eye_colorbrown   1.41e+1    1.92     1.38     0.168 
#> 15 eye_colororange  4.06e+7 3013.       0.00581  0.995 
#> 16 eye_colorred     1.61e+6 3505.       0.00408  0.997 
#> 17 eye_coloryellow  7.44e+0    1.85     1.09     0.278 
#> 18 eye_colorOther   3.57e+0    1.57     0.813    0.416
```

Finally, let’s confirm that `glmSparse` and `glm` outputs are identical.

``` r
all.equal(
  tidy(logit_formula),
  tidy(
    suppressWarnings(
      glm(gender ~ ., family = binomial, data = starwars_clean)
    )
  ),
  tolerance = 0.05
)
#> [1] TRUE
```

## Benchmark

Finally, let’s do a quick benchmark of `glmSparse` vs the alternatives.

``` r
starwars_big <- bind_rows(
  lapply(
    1:10000,
    function(i) starwars_clean
  )
)
starwars_big_x <- sparse.model.matrix(gender ~ . - 1, starwars_big)
starwars_big_y <- starwars_big$gender

bench::mark(
  glmSparse(x = starwars_big_x, y = starwars_big_y, family = binomial),
  speedglm::speedglm(gender ~ ., family = binomial(), data = starwars_big),
  suppressWarnings(glm(gender ~ ., family = binomial, data = starwars_big)),
  check = FALSE, 
  iterations = 1
) |>
  suppressWarnings() |>
  select(min:total_time)
#> # A tibble: 3 x 5
#>        min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1    18.9s    18.9s    0.0528    3.94GB    0.686
#> 2    22.8s    22.8s    0.0438    4.49GB    0.526
#> 3    34.5s    34.5s    0.0290    8.89GB    0.871
```
