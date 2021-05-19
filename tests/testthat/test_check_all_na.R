test_that("return type is a logical when one column is passed",{
  expect_equal(class(check_all_na(data.frame(x=c(1,2,1)))),
               "logical")
})

test_that("return type is a logical when two or more columns are passed",{
  expect_equal(class(check_all_na(data.frame(x=c(1,NA,1),
                                             y=c(1,2,NA)))),
               "logical")
})

test_that("return length is always 1",{
  expect_equal(length(check_all_na(data.frame(x=c(1,NA,1)))),
               1)
})

test_that("return length is always 1",{
  expect_equal(length(check_all_na(data.frame(x=c(1,1,1),
                                              y=c(1,2,1)))),
               1)
})
