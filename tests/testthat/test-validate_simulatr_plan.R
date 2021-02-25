test_that("simulatr_plan validator works as intended", {
  expect_error(validate_simulatr_plan(new_simulatr_plan()), NA)
})
