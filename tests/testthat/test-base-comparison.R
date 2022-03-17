# Toy data for tests
iris_new <- iris[sample(1:nrow(iris), nrow(iris)), ]
iris_x <- Matrix::sparse.model.matrix(Species ~ ., iris_new)
iris_y <- as.integer(iris_new$Species == "virginica")
iris_base <- as.data.frame(cbind("Species" = iris_y, as.matrix(iris_x)))
wts <- sample(1:ncol(iris), nrow(iris), replace = TRUE)
offset_vals <- sample(1:2, nrow(iris), replace = TRUE)

# Tests -------------------------------------------------------------------

test_that("glmSparse and glm vanilla binomial models are numerically identical", {
  iris_logit <- glmSparse(x = iris_x, y = iris_y, family = "binomial")
  iris_logit_base <- suppressWarnings(
    glm(Species ~ . - 1, data = iris_base, family = "binomial")
  )
  expect_equal(
    unname(coef(iris_logit)),
    unname(coef(iris_logit_base)),
    tolerance = .01
  )
  expect_equal(
    unname(residuals(iris_logit)),
    unname(residuals(iris_logit_base)),
    tolerance = .01
  )
  expect_equal(
    unname(predict(iris_logit)),
    unname(predict(iris_logit_base)),
    tolerance = .01
  )
  expect_equal(
    unname(predict(iris_logit, iris_x)),
    unname(predict(iris_logit_base, iris_base)),
    tolerance = .01
  )
  expect_equal(
    unname(predict(iris_logit, iris_x, type = "response")),
    unname(predict(iris_logit_base, iris_base, type = "response")),
    tolerance = .01
  )
})

test_that("glmSparse and glm weighted binomial models are numerically identical", {
  iris_logit <- glmSparse(x = iris_x, y = iris_y, family = "binomial", weights = wts)
  iris_logit_base <- suppressWarnings(
    glm(Species ~ . - 1, data = iris_base, family = "binomial", weights = wts)
  )
  expect_equal(
    unname(coef(iris_logit)),
    unname(coef(iris_logit_base)),
    tolerance = .01
  )
  expect_equal(
    unname(residuals(iris_logit)),
    unname(residuals(iris_logit_base)),
    tolerance = .01
  )
  expect_equal(
    unname(predict(iris_logit)),
    unname(predict(iris_logit_base)),
    tolerance = .01
  )
  expect_equal(
    unname(predict(iris_logit, iris_x)),
    unname(predict(iris_logit_base, iris_base)),
    tolerance = .01
  )
  expect_equal(
    unname(predict(iris_logit, iris_x, type = "response")),
    unname(predict(iris_logit_base, iris_base, type = "response")),
    tolerance = .01
  )
})

test_that("glmSparse and glm binomial models with `offset` specified are numerically identical", {
  iris_logit <- glmSparse(x = iris_x, y = iris_y, family = "binomial", offset = offset_vals)
  iris_logit_base <- suppressWarnings(
    glm(Species ~ . - 1, data = iris_base, family = "binomial", offset = as.numeric(offset_vals))
  )
  expect_equal(
    unname(coef(iris_logit)),
    unname(coef(iris_logit_base)),
    tolerance = .01
  )
  expect_equal(
    unname(residuals(iris_logit)),
    unname(residuals(iris_logit_base)),
    tolerance = .01
  )
  expect_equal(
    unname(predict(iris_logit)),
    unname(predict(iris_logit_base)),
    tolerance = .01
  )
  expect_equal(
    unname(predict(iris_logit, iris_x)),
    unname(predict(iris_logit_base, iris_base)),
    tolerance = .01
  )
  expect_equal(
    unname(predict(iris_logit, iris_x, type = "response")),
    unname(predict(iris_logit_base, iris_base, type = "response")),
    tolerance = .01
  )
})
