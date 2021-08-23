
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
library(bench)
library(glmSparse)
library(Matrix)
library(speedglm)

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

## Alternatives

The following example compares `glmSparse` to `glm` and `speedglm` using
a sub-sample of the [PUMS data for the 1990
census](https://www.census.gov/data/datasets/1990/dec/pums.html) to
classify an individual’s sex. This provides a tractable example of a
case where there will be significant sparsity, as each variable is coded
as a factor variable, often with multiple levels. When fully expanded,
the model matrix will be fairly sparse and `glmSparse` can take
advantage of this. The following gives us a quick overview of the
modeling data

``` r
str(pums_dat[, c(1, 4:7, 56)])
#> tibble [50,000 x 6] (S3: tbl_df/tbl/data.frame)
#>  $ dAge    : Factor w/ 8 levels "0","1","2","3",..: 6 7 4 5 8 2 2 5 7 4 ...
#>  $ iAvail  : Factor w/ 5 levels "0","1","2","3",..: 1 1 1 1 1 1 1 1 1 1 ...
#>  $ iCitizen: Factor w/ 5 levels "0","1","2","3",..: 1 1 1 1 1 1 1 1 1 1 ...
#>  $ iClass  : Factor w/ 10 levels "0","1","2","3",..: 6 8 8 2 1 1 1 7 2 2 ...
#>  $ dDepart : Factor w/ 6 levels "0","1","2","3",..: 4 6 5 4 1 1 1 1 1 1 ...
#>  $ iSex    : num [1:50000] 1 1 1 1 1 1 0 1 1 0 ...
```

Using the `bench` package, we can benchmark the fitting time and memory
usage of `glmSparse` compared to its `glm` and `speedglm` counterparts.

``` r
benchmarks <- mark(
  glm(iSex ~ .,
        data = pums_dat[, c(1, 4:7, 56)],
        family = binomial),
  glmSparse(iSex ~ .,
              data = pums_dat[, c(1, 4:7, 56)],
              family = binomial),
  speedglm(iSex ~ .,
             data = pums_dat[, c(1, 4:7, 56)],
             family = binomial()),
  iterations = 100,
  check = FALSE
)

# Arrange implementations from fastest (median) to slowest
benchmarks %>%
  mutate(implementation = c("glm", "glmSparse", "speedglm")) %>%
  `[`(, c(14, 2:3, 5, 7:9)) %>%
  arrange(median) %>%
  print()
#> # A tibble: 3 x 7
#>   implementation      min   median mem_alloc n_itr  n_gc total_time
#>   <chr>          <bch:tm> <bch:tm> <bch:byt> <int> <dbl>   <bch:tm>
#> 1 glmSparse         218ms    257ms     104MB   100   157      26.3s
#> 2 speedglm          259ms    326ms     126MB   100   211      34.6s
#> 3 glm               469ms    596ms     194MB   100   393      57.4s
```
