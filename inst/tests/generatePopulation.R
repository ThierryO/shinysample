context("Generating a population")

test_that("generatePopulation() tests the sanity of the population size", {
  expect_that(generatePopulation(n = "100"), throws_error("n must be a number"))
  expect_that(generatePopulation(n = -9999), throws_error("n must be strict positive"))
  expect_that(generatePopulation(n = -1), throws_error("n must be strict positive"))
  expect_that(generatePopulation(n = 0), throws_error("n must be strict positive"))
  expect_that(generatePopulation(n = 1.5), throws_error("n must be integer"))
  expect_that(generatePopulation(n = integer(0)), throws_error("n must be a single value"))
  expect_that(generatePopulation(n = c(100, 200)), throws_error("n must be a single value"))
})

test_that("generatePopulation() tests the sanity of the dimensions", {
  expect_that(generatePopulation(dimension = "100"), throws_error("dimension must be a matrix"))
  expect_that(generatePopulation(dimension = matrix("100")), throws_error("dimension must be numeric"))
  expect_that(generatePopulation(dimension = matrix(-1)), throws_error("all proportions in dimension must be positive"))
  expect_that(generatePopulation(dimension = matrix(0)), throws_error("at least one proportion in dimension must be strict positive"))
  expect_that(generatePopulation(dimension = matrix(integer(0))), throws_error("dimension must have at least one row and one column"))
  expect_that(generatePopulation(dimension = matrix(integer(0), nrow = 0)), throws_error("dimension must have at least one row and one column"))
  expect_that(generatePopulation(dimension = matrix(integer(0), nrow = 1)), throws_error("dimension must have at least one row and one column"))
})

test_that("generatePopulation() tests the sanity of the continuous means", {
  expect_that(generatePopulation(continuous.mean = "100"), throws_error("continuous.mean must be a numeric vector"))
  expect_that(generatePopulation(continuous.mean = 1), throws_error("continuous.mean must have length >= 2"))
  expect_that(generatePopulation(continuous.mean = integer(0)), throws_error("continuous.mean must have length >= 2"))
})

test_that("generatePopulation() tests the sanity of the continuous covariance matrix", {
  expect_that(generatePopulation(continuous.var = "100"), throws_error("continuous.var must be a matrix"))
  expect_that(generatePopulation(continuous.var = matrix("100")), throws_error("continuous.var must be a numeric matrix"))
  expect_that(generatePopulation(continuous.var = matrix(1)), throws_error("continuous.var must have the same number of columns and rows as the number of elements in continuous.mean"))
  expect_that(generatePopulation(continuous.mean = integer(0)), throws_error("continuous.mean must have length >= 2"))
})

test_that("generatePopulation() tests the sanity of the categorical proportions", {
  expect_that(generatePopulation(categorical.proportion = "100"), throws_error("categorical.proportion must be a numeric vector"))
  expect_that(generatePopulation(categorical.proportion = integer(0)), throws_error("categorical.proportion must have a least one proportion"))
  expect_that(generatePopulation(categorical.proportion = c(-1, 10)), throws_error("All elements of categorical.proportion must be strict positive"))
  expect_that(generatePopulation(categorical.proportion = c(0, 10)), throws_error("All elements of categorical.proportion must be strict positive"))
})

test_that("generatePopulation() return a data.frame", {
  expect_that(generatePopulation(), is_a("data.frame"))
  expect_that(colnames(generatePopulation()), is_identical_to(c("X1", "X2", "X3", "Categorical", "ID")))
  n <- 10L
  expect_that(nrow(generatePopulation(n = n)), is_identical_to(n))
  n <- 10
  expect_that(nrow(generatePopulation(n = n)), equals(n))
})
