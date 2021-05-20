test_that("return type is a logical when one column is passed",{
  expect_equal(class(check_all_numeric(data.frame(x=c(1,2,1)))),
               "logical")
})

test_that("return type is a logical when two or more columns are passed",{
  expect_equal(
    class(check_all_numeric(data.frame(x=c(1,1,1),y=c(1,2,1)))),
    "logical")
  })

test_that("return length is always 1",{
  expect_equal(length(check_all_numeric(data.frame(x=c(1,2,1)))),
               1)
})


test_that("return length is always 1",{
  expect_equal(length(check_all_numeric(data.frame(x=c(1,1,1),y=c(1,2,1)))),
               1)
})

test_that("return value for all numeric variables is FALSE",{
  expect_equal(check_all_numeric(data.frame(x=c(1,1,1),y=c(1,2,1))),
               FALSE)
})

test_that("return value for some character variables is TRUE",{
  expect_equal(check_all_numeric(data.frame(x=c(1,1,1),y=c("1","2","1"))),
               TRUE)
})

test_that("return value for some factor variables is TRUE",{
  expect_equal(check_all_numeric(data.frame(x=c(1,1,1),y=as.factor(c(1,2,1)))),
               TRUE)
})