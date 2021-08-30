library(testthat)
library(data.table)

devtools::load_all()

set.seed(123)
sim_data = data.table(
  response = rbinom(n = 1000, size = 1, prob = 0.5),
  feat1 = sample(LETTERS, size = 1000, replace = T),
  feat2 = sample(c(1:100), size = 1000, replace = T)
)

test_that("test split function works correctly", {

  output_data = split_train_dt(train_dat = sim_data)
  expect_equal(nrow(sim_data), 1000)
  expect_equal(nrow(output_data$train_dt), 600)
  expect_equal(nrow(output_data$test_dt), 200)
  expect_equal(nrow(output_data$valid_dt), 200)

  output_data2 = split_train_dt(sim_data, partitions = 2, prop = c(0.7,0.3))
  expect_equal(nrow(output_data2$train_dt), 700)
  expect_equal(nrow(output_data2$test_dt), 300)
  expect_null(output_data2$valid_dt)

})

test_that("test split function with incorrect params", {

  expect_error(split_train_dt())
  expect_error(split_train_dt(sim_data, partitions = 4))
  expect_error(split_train_dt(sim_data, partitions = 3, prop = c(0.1,0.1,0.1)))
  expect_error(split_train_dt(sim_data, partitions = 2, prop = c(0.8,0.1,0.1)))
  expect_error(split_train_dt(sim_data, partitions = 3, prop = c(0.8,0.1)))

})
