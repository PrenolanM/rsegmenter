
testthat::test_that("Correct column is returned from max_score",{
  testthat::expect_equal(
    rsegmenter::max_score(data.frame(col1 = c(1,2,3,4),
                                     col2 = c(4,3,2,1),
                                     col3 = c(2,4,1,3))),
    c(2,3,1,1))
  })


testthat::test_that("Correct column is returned from max_score when there are multiple max values",{
  testthat::expect_equal(
    rsegmenter::max_score(data.frame(col1 = c(1,4,3,4),
                                     col2 = c(4,3,2,1),
                                     col3 = c(4,4,1,3))),
    c(2,1,1,1))
})