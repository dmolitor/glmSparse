
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

The primary feature of `glmSparse()` is that it allows the user the
create a sparse model matrix and fit a generalized linear model using
this sparse matrix. In cases where the model matrix is highly sparse
this will be much more memory/computationally efficient. For a more
in-depth discussion of sparse matrices and their benefits, see [this
write-up](https://cran.r-project.org/web/packages/Matrix/vignettes/sparseModels.pdf).
Using a quick example we can confirm that the results of `glm()` and
`glmSparse()` are the same.

``` r
library(broom)
library(glmSparse)
library(Matrix)

x <- sparse.model.matrix(mpg ~ ., data = mtcars)
y <- mtcars$mpg

# stats::glm implementation
glmBase <- glm(mpg ~ ., 
               family = gaussian, 
               data = mtcars)
# glmSparse formula interface
glmSparse_form <- glmSparse(mpg ~ .,
                            family = gaussian,
                            data = mtcars)
# glmSparse alternate interface
glmSparse_alt <- glmSparse(x = x,
                           y = y,
                           family = gaussian)

# Compare the coefficient tables
tidy(glmBase)
#> # A tibble: 11 x 5
#>    term        estimate std.error statistic p.value
#>    <chr>          <dbl>     <dbl>     <dbl>   <dbl>
#>  1 (Intercept)  12.3      18.7        0.657  0.518 
#>  2 cyl          -0.111     1.05      -0.107  0.916 
#>  3 disp          0.0133    0.0179     0.747  0.463 
#>  4 hp           -0.0215    0.0218    -0.987  0.335 
#>  5 drat          0.787     1.64       0.481  0.635 
#>  6 wt           -3.72      1.89      -1.96   0.0633
#>  7 qsec          0.821     0.731      1.12   0.274 
#>  8 vs            0.318     2.10       0.151  0.881 
#>  9 am            2.52      2.06       1.23   0.234 
#> 10 gear          0.655     1.49       0.439  0.665 
#> 11 carb         -0.199     0.829     -0.241  0.812
tidy(glmSparse_form)
#> # A tibble: 11 x 5
#>    term        estimate std.error statistic p.value
#>    <chr>          <dbl>     <dbl>     <dbl>   <dbl>
#>  1 (Intercept)  12.3      18.7        0.657  0.518 
#>  2 cyl          -0.111     1.05      -0.107  0.916 
#>  3 disp          0.0133    0.0179     0.747  0.463 
#>  4 hp           -0.0215    0.0218    -0.987  0.335 
#>  5 drat          0.787     1.64       0.481  0.635 
#>  6 wt           -3.72      1.89      -1.96   0.0633
#>  7 qsec          0.821     0.731      1.12   0.274 
#>  8 vs            0.318     2.10       0.151  0.881 
#>  9 am            2.52      2.06       1.23   0.234 
#> 10 gear          0.655     1.49       0.439  0.665 
#> 11 carb         -0.199     0.829     -0.241  0.812
tidy(glmSparse_alt)
#> # A tibble: 11 x 5
#>    term        estimate std.error statistic p.value
#>    <chr>          <dbl>     <dbl>     <dbl>   <dbl>
#>  1 (Intercept)  12.3      18.7        0.657  0.518 
#>  2 cyl          -0.111     1.05      -0.107  0.916 
#>  3 disp          0.0133    0.0179     0.747  0.463 
#>  4 hp           -0.0215    0.0218    -0.987  0.335 
#>  5 drat          0.787     1.64       0.481  0.635 
#>  6 wt           -3.72      1.89      -1.96   0.0633
#>  7 qsec          0.821     0.731      1.12   0.274 
#>  8 vs            0.318     2.10       0.151  0.881 
#>  9 am            2.52      2.06       1.23   0.234 
#> 10 gear          0.655     1.49       0.439  0.665 
#> 11 carb         -0.199     0.829     -0.241  0.812
```

The results are identical!

## Why use this?

The primary reason to use this implementation is in the case of an
extremely large model matrix that is fairly sparse that will cause
memory issues if stored in standard dense format. When dealing with
moderate sized datasets, there is little to no reason to prefer this
vs. the `glm()` implementation. Additionally, I have done no comparison
to the implementations found in the `speedglm` or `bigglm` packages;
both of which claim to address similar situations, potentially with
better results. If these packages indeed have faster implementations and
can handle sparse model matrices equally well, there is no reason to
utilize `glmSparse()`. In other words, this is tbd.
