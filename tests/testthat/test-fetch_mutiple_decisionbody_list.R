test_that("result is a dataframe", {
  result <- fetch_multiple_decisionbody_list()
  expect_s3_class(result, "data.frame")
})
