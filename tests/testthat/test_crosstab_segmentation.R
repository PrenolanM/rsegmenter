testthat::test_that("Correct segments are assigned from crosstab_segmentation",{
  testthat::expect_equal(
    rsegmenter::crosstab_segmentation(data.frame(col1 = c(1,2,1,4),
                                                 col2 = c(4,3,4,1),
                                                 col3 = c(2,5,2,3)
                                                 )
                                      ),
    data.frame(Crosstab_Segment = c(3,2,3,1)))
})