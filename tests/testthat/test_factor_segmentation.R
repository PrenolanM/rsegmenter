
testthat::test_that("Factoring produces correct number of solutions",{
testthat::expect_equal(
  rsegmenter::factor_segmentation(rsegmenter::test_seg_unlabelled,
                                  vars = c("seg1","seg2","seg3","seg4",
                                           "seg5","seg6","seg7","seg8",
                                           "seg9","seg10"),
                                  weight_var = "weight",
                                  num_sols = c(2:3),
                                  scores = FALSE,
                                  fac_assign = "avg_loading") %>% 
    length(),2)
  })

testthat::test_that("Factoring produces the correct segment assignments for avg loading",{
  testthat::expect_equal(
    rsegmenter::factor_segmentation(rsegmenter::test_seg_unlabelled,
                                    vars = c("seg1","seg2","seg3","seg4",
                                             "seg5","seg6","seg7","seg8",
                                             "seg9","seg10"),
                                    weight_var = "weight",
                                    num_sols = c(3),
                                    scores = FALSE,
                                    fac_assign = "avg_loading") %>% 
      rsegmenter::extract_factor_segments() %>% 
      dplyr::slice_head(n=10) %>% 
      unlist() %>% 
      unname(),
    c(2,3,2,2,2,2,2,1,3,3)
  )
})

testthat::test_that("Factoring produces the correct segment assignments for max score",{
  testthat::expect_equal(
    rsegmenter::factor_segmentation(rsegmenter::test_seg_unlabelled,
                                    vars = c("seg1","seg2","seg3","seg4",
                                             "seg5","seg6","seg7","seg8",
                                             "seg9","seg10"),
                                    weight_var = "weight",
                                    num_sols = c(3),
                                    scores = TRUE,
                                    fac_assign = "max_score") %>% 
      rsegmenter::extract_factor_segments() %>% 
      dplyr::slice_head(n=10) %>% 
      unlist() %>% 
      unname(),
    c(2,3,2,2,2,2,2,1,3,2)
    )
  })

testthat::test_that("Correct number of segments are extracted",{
  testthat::expect_equal(
    rsegmenter::test_seg_unlabelled %>% 
      rsegmenter::factor_segmentation(vars = c("seg1","seg2","seg3","seg4",
                                               "seg5","seg6","seg7","seg8",
                                               "seg9","seg10"),
                                      weight_var = "weight",num_sols = c(2:6),
                                      scores = FALSE,
                                      fac_assign = "avg_loading") %>% 
      rsegmenter::extract_factor_segments() %>% 
      ncol(),5)
  })


rsegmenter::factor_segmentation(rsegmenter::test_seg_unlabelled,
                                vars = c("seg1","seg2","seg3","seg4",
                                         "seg5","seg6","seg7","seg8",
                                         "seg9","seg10"),
                                weight_var = "weight",
                                num_sols = c(3),
                                scores = TRUE,
                                fac_assign = "max_score") %>% 
  rsegmenter::extract_factor_segments() %>% 
  dplyr::slice_head(n=10) %>% 
  unlist() %>% 
  unname()