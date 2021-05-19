
test_that("variable with the same value gets recoded to all 1's",{
  expect_equal(top_two_box(data.frame(x=rep(sample(0:1,1),3)),"x"),
               data.frame(x_top_2_box=c(1,1,1)))
})

test_that("variable with 2 unique values gets recoded to all 1's",{
  expect_equal(top_two_box(data.frame(x=c(1,2,1),
                                  y=c(6,4,6)),c("x","y")),
               data.frame(x_top_2_box=c(1,1,1),
                          y_top_2_box=c(1,1,1)))
})

test_that("variable with more than 2 unique values gets recoded to all 0's and 1's",{
  expect_equal(top_two_box(data.frame(x=c(1,2,0),
                                      y=c(6,4,5)),c("x","y")),
               data.frame(x_top_2_box=c(1,1,0),
                          y_top_2_box=c(1,0,1)))
})

test_that("return type is a data.frame when one column is passed",{
  expect_equal(class(top_two_box(data.frame(x=c(1,2,1)),"x")),
               "data.frame")
})

test_that("return type is a data.frame when two or more columns are passed",{
  expect_equal(class(top_two_box(data.frame(x=c(1,2,1),
                                            y=c(1,2,1)),
                                 c("x","y"))),
               "data.frame")
})