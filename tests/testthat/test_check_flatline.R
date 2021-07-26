test_that("proportion for 1 variable input is 1 for all rows",{
  expect_equal(check_flatline(data.frame(c(1,2,3)),1),
               data.frame(Prop_Flatline=c(1,1,1)))
})

test_that("proportion for 2 equal variables is 1 for rows",{
  expect_equal(check_flatline(data.frame(x=c(1,2,3),
                                         y=c(1,2,3)),c("x","y")),
               data.frame(Prop_Flatline=c(1,1,1)))
})

test_that("proportion for 3 variables",{
  expect_equal(check_flatline(data.frame(x=c(1,2,1),
                                         y=c(2,2,2),
                                         z=c(1,2,3)),c("x","y","z")),
               data.frame(Prop_Flatline=c(2/3,1,1/3)))
})
