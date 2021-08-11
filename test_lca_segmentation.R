
testthat::test_that("LCA produces correct number of solutions",{
  testthat::expect_equal(
    rsegmenter::lca_segmentation(rsegmenter::test_seg_unlabelled,
                                 vars = c("seg1","seg2","seg3","seg4",
                                          "seg5","seg6","seg7","seg8",
                                          "seg9","seg10"),
                                 num_sols = c(2:3),
                                 maxiter = 1000,
                                 tol=1e-10,
                                 na.rm=TRUE,
                                 nrep=1) %>% 
      length(),2)
  })

testthat::test_that("LCA produces the correct segment assignments",{
  testthat::expect_equal(
    rsegmenter::lca_segmentation(rsegmenter::test_seg_unlabelled,
                                 vars = c("seg1","seg2","seg3","seg4",
                                          "seg5","seg6","seg7","seg8",
                                          "seg9","seg10"),
                                 num_sols = c(3),
                                 maxiter = 1000,
                                 tol=1e-10,
                                 na.rm=TRUE,
                                 nrep=1) %>% 
      rsegmenter::extract_lca_segments() %>% 
      dplyr::slice_head(n=10) %>% 
      unlist() %>% 
      unname(),
    c(2,3,3,3,2,3,3,2,3,3)
  )
})

testthat::test_that("Correct number of segments are extracted",{
  testthat::expect_equal(
    rsegmenter::lca_segmentation(rsegmenter::test_seg_unlabelled,
                                 vars = c("seg1","seg2","seg3","seg4",
                                          "seg5","seg6","seg7","seg8",
                                          "seg9","seg10"),
                                 num_sols = c(2:6),
                                 maxiter = 1000,
                                 tol=1e-10,
                                 na.rm=TRUE,
                                 nrep=1) %>% 
      rsegmenter::extract_lca_segments() %>% 
      ncol(),5)
})
