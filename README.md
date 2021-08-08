
<!-- README.md is generated from README.Rmd. Please edit that file -->

# glmSparse

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of glmSparse is to provide common methods such as `print`,
`summary`, `broom::tidy`, for sparse/dense GLM models implemented in the
[MatrixModels](https://cran.r-project.org/web/packages/MatrixModels/index.html)
package. For more information on this particular implementation of GLM
models, see the documentation for `MatrixModels::glm4()`.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("dmolitor/glmSparse")
```

## Sparse vs. Dense

The primary feature of `glm4` is that it allows the user the create a
sparse model matrix and fit a generalized linear model using this sparse
matrix. In cases where the model matrix is highly sparse this will be
much more memory/computationally efficient. For a more in-depth
discussion of sparse matrices and their benefits, see [this
write-up](https://cran.r-project.org/web/packages/Matrix/vignettes/sparseModels.pdf).
Using one of the examples from the ModelMatrix
[pdf](https://cran.r-project.org/web/packages/MatrixModels/MatrixModels.pdf)
we can see some comparison between GLM implementations.

``` r
library(glmSparse)

## From example(glm): -----------------

## Dobson (1990) Page 93: Randomized Controlled Trial :
trial <- data.frame(counts=c(18,17,15,20,10,20,25,13,12),
                        outcome=gl(3,1,9,labels=LETTERS[1:3]),
                        treatment=gl(3,3,labels=letters[1:3]))

# stats::glm implementation
glmDense <- glm(counts ~ outcome + treatment, 
                family = poisson, 
                data = trial)
# glm4 implementation (sparse)
glmSparse <- glm4(counts ~ outcome + treatment, 
                  family = poisson, 
                  data = trial,
                  sparse = TRUE)

# Compare the coefficient tables
dplyr::mutate(broom::tidy(glmDense),
              dplyr::across(where(is.numeric), round, digits = 7))
#> # A tibble: 5 x 5
#>   term        estimate std.error statistic p.value
#>   <chr>          <dbl>     <dbl>     <dbl>   <dbl>
#> 1 (Intercept)    3.04      0.171     17.8   0     
#> 2 outcomeB      -0.454     0.202     -2.25  0.0246
#> 3 outcomeC      -0.293     0.193     -1.52  0.128 
#> 4 treatmentb     0         0.2        0     1     
#> 5 treatmentc     0         0.2        0     1
dplyr::mutate(broom::tidy(glmDense),
              dplyr::across(where(is.numeric), round, digits = 7))
#> # A tibble: 5 x 5
#>   term        estimate std.error statistic p.value
#>   <chr>          <dbl>     <dbl>     <dbl>   <dbl>
#> 1 (Intercept)    3.04      0.171     17.8   0     
#> 2 outcomeB      -0.454     0.202     -2.25  0.0246
#> 3 outcomeC      -0.293     0.193     -1.52  0.128 
#> 4 treatmentb     0         0.2        0     1     
#> 5 treatmentc     0         0.2        0     1
```

Although the results aren’t numerically identical, they are identical
when rounded to 7 digits, so their precision is exceedingly close.

## Why use this?

The primary reason to use this implementation is in the case of an
extremely large model matrix that is fairly sparse that will cause
memory issues if stored in standard dense format. When dealing with
moderate sized datasets, there is little to no reason to prefer this
vs. the `stats::glm` implementation. Additionally, I have done no
comparison to the implementations found in the `speedglm` or `bigglm`
packages; both of which claim to address similar situations, potentially
with better results. If these packages indeed have faster
implementations and can handle sparse model matrices equally well, there
is no reason to utilize `glm4()`. In other words, this is tbd.
