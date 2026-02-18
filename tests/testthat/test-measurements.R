library(testthat)

test_that("anthro_measurements returns values for z=0 for all methods", {
  methods <- c("length", "weight", "bmi", "headc")
  for (m in methods) {
    out <- anthro_measurements(sex = 1, age = 365, requested_z = 0, measurement_method = m)
    col <- names(out)[1]
    expect_true(is.numeric(out[[col]]))
    expect_false(is.na(out[[col]]))
  }
})

test_that("anthro_measurements handles extreme z with and without correction", {
  methods <- c("length", "weight", "bmi", "headc")
  for (m in methods) {
    out_false <- anthro_measurements(sex = 1, age = 365, requested_z = 4, measurement_method = m, correct_extreme = FALSE)
    colf <- names(out_false)[1]
    expect_true(is.na(out_false[[colf]]), info = paste(m, "should be NA without correction"))

    out_true <- anthro_measurements(sex = 1, age = 365, requested_z = 4, measurement_method = m, correct_extreme = TRUE)
    colt <- names(out_true)[1]
    expect_true(is.numeric(out_true[[colt]]), info = paste(m, "should return numeric with correction"))
    expect_false(is.na(out_true[[colt]]), info = paste(m, "should not be NA with correction"))
  }
})
