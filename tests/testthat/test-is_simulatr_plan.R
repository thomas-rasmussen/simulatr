test_that("works as intended", {
  expect_equal(is_simulatr_plan(new_simulatr_plan()), TRUE)
  expect_equal(is_simulatr_plan(list()), FALSE)
})
