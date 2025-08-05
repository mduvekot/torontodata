test_that("result is an integer", {
  result <- lookup_decision_body_id()
  expect_type(result, "integer")
})

test_that("City Council is 2462", {
  result <- lookup_decision_body_id("City Council")
  expect_equal(result, 2462)
})
