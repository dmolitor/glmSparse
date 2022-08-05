# Toy data for tests
iris_new <- iris[sample(1:nrow(iris), nrow(iris)), ]
iris_x <- Matrix::sparse.model.matrix(Species ~ ., iris_new)
iris_y <- as.integer(iris_new$Species == "virginica")
iris_base <- as.data.frame(cbind("Species" = iris_y, as.matrix(iris_x)))
wts <- sample(1:ncol(iris), nrow(iris), replace = TRUE)

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
  offset_vals <- sample(1:2, nrow(iris), replace = TRUE)
  iris_logit <- glmSparse(x = iris_x, y = iris_y, family = "binomial", offset = rep(1:2, nrow(iris)/2))
  iris_logit_base <- suppressWarnings(
    glm(Species ~ . - 1, data = iris_base, family = "binomial", offset = as.numeric(rep(1:2, nrow(iris)/2)))
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

test_that(
  "glmSparse and glm binomial predict numerically identical values on unlabelled data",
  {
    mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
    mydata$rank <- factor(mydata$rank)
    newdata1 <- with(mydata, data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))
    mylogit_base <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")
    mylogit_sparse <- glmSparse(admit ~ gre + gpa + rank, data = mydata, family = "binomial")
    expect_equal(
      predict(mylogit_base, newdata = newdata1, type = "response"),
      predict(mylogit_sparse, newdata1, type = "response"),
      tolerance = .001
    )
  }
)

test_that(
  "glmSparse and glm binomial predict numerically identical values on unlabelled data with `offset` specified",
  {
    mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
    mydata$rank <- factor(mydata$rank)
    newdata1 <- with(mydata, data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))
    mylogit_base <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial", offset = rep(c(2, 3), 200))
    mylogit_sparse <- glmSparse(admit ~ gre + gpa + rank, data = mydata, family = "binomial", offset = rep(c(2, 3), 200))
    expect_equal(
      predict(mylogit_base, newdata = newdata1, type = "response"),
      predict(mylogit_sparse, newdata1, type = "response"),
      tolerance = .001
    )
  }
)
