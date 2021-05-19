
test_that("1 row 1 column df doesnt change value",{
  expect_equal(reverse_scale(data.frame(x=c(1)),1),
               data.frame(x_reverse_scale=1))
})

test_that("rescale works no matter the values present in each variable",{
  expect_equal(reverse_scale(data.frame(x=c(1,2,3),
                                        y=c(0,2,4)),c("x","y")),
               data.frame(x_reverse_scale=c(3,2,1),
                          y_reverse_scale=c(4,2,0)))
})
