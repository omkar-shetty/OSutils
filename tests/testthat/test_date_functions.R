library(testthat)

devtools::load_all()

test_that("test date to idnt function with a single value", {
    dt = as.Date('2021-08-30')
    dt_idnt = convert_date_to_idnt(dt)
    expect_equal(dt_idnt, 2021241)
    expect_type(dt_idnt, 'double')
    dt_posixct = as.POSIXct.Date(dt)
    dt_idnt_posixct = convert_date_to_idnt(dt_posixct)
    expect_equal(dt_idnt_posixct, 2021241)
    expect_type(dt_idnt_posixct, 'double')

    #Check with a ridiculous value
    expect_error(convert_date_to_idnt(as.Date('2021-13-40')))
    expect_error(convert_date_to_idnt(as.Date('2023-40')))

})

test_that("test date to idnt function with a vector", {
  dt = c(as.Date('2021-08-30'), as.Date('2020-01-01'), as.Date('2022-12-31'))
  dt_idnt = convert_date_to_idnt(dt)
  expect_equal(dt_idnt, c(2021241, 2020000, 2022364))
  expect_type(dt_idnt, 'double')
  dt_posixct = as.POSIXct.Date(dt)
  dt_idnt_posixct = convert_date_to_idnt(dt_posixct)
  expect_equal(dt_idnt_posixct, c(2021241, 2020000, 2022364))
  expect_type(dt_idnt_posixct, 'double')

})
