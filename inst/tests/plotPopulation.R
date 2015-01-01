context("Generating a population")

set.seed(12345)
population <- generatePopulation(n = 100)

test_that("plotPopulation() tests the sanity of the number of columns", {
  expect_that(plotPopulation(population, cols = "10"), throws_error("cols must be a number"))
  expect_that(plotPopulation(population, cols = -10), throws_error("cols must be strict positive"))
  expect_that(plotPopulation(population, cols = 0), throws_error("cols must be strict positive"))
  expect_that(plotPopulation(population, cols = 1.5), throws_error("cols must be integer"))
  expect_that(plotPopulation(population, cols = c(10, 20)), throws_error("cols must be a single number"))
})

test_that("plotPopulation() tests the sanity of the number of rows", {
  expect_that(plotPopulation(population, rows = "10"), throws_error("rows must be a number"))
  expect_that(plotPopulation(population, rows = -10), throws_error("rows must be strict positive"))
  expect_that(plotPopulation(population, rows = 0), throws_error("rows must be strict positive"))
  expect_that(plotPopulation(population, rows = 1.5), throws_error("rows must be integer"))
  expect_that(plotPopulation(population, rows = c(10, 20)), throws_error("rows must be a single number"))
})

test_that("plotPopulation() tests the sanity of the population", {
  expect_that(plotPopulation(population = "10"), throws_error("population must be a data.frame"))
  expect_that(plotPopulation(population = data.frame(integer(0))), throws_error("empty population"))
  junk <- population
  junk$Height <- "AA"
  expect_that(plotPopulation(population = junk), throws_error("population$Height must be numeric"))
  junk$Height <- NA
  expect_that(plotPopulation(population = junk), throws_error("the columns Width, Height, Categorical, Selected and ID from population should not contain missing values"))
  junk$Height <- NULL
  expect_that(plotPopulation(population = junk), throws_error("population must contain at least the columns Width, Height, Categorical, Selected and ID"))
  junk <- population
  junk$Width <- "AA"
  expect_that(plotPopulation(population = junk), throws_error("population$Width must be numeric"))
  junk$Width <- NA
  expect_that(plotPopulation(population = junk), throws_error("the columns Width, Height, Categorical, Selected and ID from population should not contain missing values"))
  junk$Width <- NULL
  expect_that(plotPopulation(population = junk), throws_error("population must contain at least the columns Width, Height, Categorical, Selected and ID"))
  junk <- population
  junk$ID <- "AA"
  expect_that(plotPopulation(population = junk), throws_error("population$ID must contain unique values"))
  junk$ID <- NULL
  expect_that(plotPopulation(population = junk), throws_error("population must contain at least the columns Width, Height, Categorical, Selected and ID"))
  junk <- population
  junk$Categorical <- "AA"
  expect_that(plotPopulation(population = junk), gives_warning("population$Categorical converted to a factor"))
  junk$Categorical <- NA
  expect_that(plotPopulation(population = junk), throws_error("the columns Width, Height, Categorical, Selected and ID from population should not contain missing values"))
  junk$Categorical <- NULL
  expect_that(plotPopulation(population = junk), throws_error("population must contain at least the columns Width, Height, Categorical, Selected and ID"))
  junk <- population
  junk$Selected <- "AA"
  expect_that(plotPopulation(population = junk), throws_error("population$Selected must be logical"))
  junk$Selected <- NA
  expect_that(plotPopulation(population = junk), throws_error("the columns Width, Height, Categorical, Selected and ID from population should not contain missing values"))
  junk$Selected <- NULL
  expect_that(plotPopulation(population = junk), throws_error("population must contain at least the columns Width, Height, Categorical, Selected and ID"))

})

test_that("plotPopulation() tests the combined sanity of the number of rows and colums", {
  expect_that(plotPopulation(population, rows = 1, cols = 1), throws_error("number of rows and cols not adequat to cover the entire population"))
})
