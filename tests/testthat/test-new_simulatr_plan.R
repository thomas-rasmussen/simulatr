test_that("simulatr_plan constuctor works as intended", {
  expect_error(new_simulatr_plan(), NA)
  # Other classes, eg. data.table, has typeof list so also passes is.list() check
  expect_error(new_simulatr_plan(data.table()), NA)
  # List with the wrong list structure all passes check
  expect_error(new_simulatr_plan(list("passes check")), NA)
})
