
test_that("variable with the same value gets recoded to all 1's",{
  expect_equal(top_box(data.frame(x=rep(sample(1:100,1),3)),"x"),
               data.frame(x_top_box=c(1,1,1)))
})

test_that("variable with more than 1 unique values gets recoded",{
  expect_equal(top_box(data.frame(x=c(1,2,3),
                                  y=c(6,4,6)),c("x","y")),
               data.frame(x_top_box=c(0,0,1),
                          y_top_box=c(1,0,1)))
})

test_that("return type is a data.frame when one column is passed",{
  expect_equal(class(top_box(data.frame(x=c(1,2,1)),"x")),
               "data.frame")
})

test_that("return type is a data.frame when two or more columns are passed",{
  expect_equal(class(top_box(data.frame(x=c(1,2,1),
                                        y=c(1,2,1)),
                             c("x","y"))),
               "data.frame")
})