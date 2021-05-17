test_that("proportion for 1 variable input is 1 for all rows",{
  expect_equal(check_flatline(data.frame(x=c(1,2,3))),c(1,1,1))
})