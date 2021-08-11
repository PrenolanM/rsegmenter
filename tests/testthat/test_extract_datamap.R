testthat::test_that("Datamap produces the correct structure",{
  testthat::expect_equal(
    rsegmenter::extract_datamap(rsegmenter::test_seg_labelled),
    rsegmenter::test_datamap)
})