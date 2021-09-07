
testthat::test_that("Raw profile tables",{
testthat::expect_equal(
  rsegmenter::test_seg_unlabelled %>% 
    dplyr::mutate(rsegmenter::factor_segmentation(rsegmenter::test_seg_unlabelled,
                                                  vars = c("seg1","seg2","seg3","seg4",
                                                           "seg5","seg6","seg7","seg8",
                                                           "seg9","seg10"),
                                                  weight_var = "weight",
                                                  num_sols = c(5),
                                                  fac_assign = "avg_loading") %>%
                    rsegmenter::extract_factor_segments()) %>% 
    rsegmenter::profile_table_raw(category_vars = c("seg1","seg2","seg3","seg4",
                                                    "seg5","seg6","seg7","seg8",
                                                    "seg9","seg10"),
                                  numeric_vars = NULL,
                                  banner_var = c("Factor_Cluster_Soln_5")) %>% 
    dplyr::arrange(Variable_Name,Value_Code),
    rsegmenter::raw_tab %>% 
    dplyr::arrange(Variable_Name,Value_Code))
  })

testthat::test_that("Col % profile tables",{
  testthat::expect_equal(
    rsegmenter::test_seg_unlabelled %>% 
      dplyr::mutate(rsegmenter::factor_segmentation(rsegmenter::test_seg_unlabelled,
                                                    vars = c("seg1","seg2","seg3","seg4",
                                                             "seg5","seg6","seg7","seg8",
                                                             "seg9","seg10"),
                                                    weight_var = "weight",
                                                    num_sols = c(5),
                                                    fac_assign = "avg_loading") %>%
                      rsegmenter::extract_factor_segments()) %>% 
      rsegmenter::profile_table_col_perc(category_vars = c("seg1","seg2","seg3","seg4",
                                                           "seg5","seg6","seg7","seg8",
                                                           "seg9","seg10"),
                                         numeric_vars = NULL,
                                         banner_var = c("Factor_Cluster_Soln_5")) %>% 
      dplyr::arrange(Variable_Name,Value_Code),
    rsegmenter::col_tab %>% 
      dplyr::arrange(Variable_Name,Value_Code))
})

testthat::test_that("Row % profile tables",{
  testthat::expect_equal(
    rsegmenter::test_seg_unlabelled %>% 
      dplyr::mutate(rsegmenter::factor_segmentation(rsegmenter::test_seg_unlabelled,
                                                    vars = c("seg1","seg2","seg3","seg4",
                                                             "seg5","seg6","seg7","seg8",
                                                             "seg9","seg10"),
                                                    weight_var = "weight",
                                                    num_sols = c(5),
                                                    fac_assign = "avg_loading") %>%
                      rsegmenter::extract_factor_segments()) %>% 
      rsegmenter::profile_table_row_perc(category_vars = c("seg1","seg2","seg3","seg4",
                                                           "seg5","seg6","seg7","seg8",
                                                           "seg9","seg10"),
                                         numeric_vars = NULL,
                                         banner_var = c("Factor_Cluster_Soln_5")) %>% 
      dplyr::arrange(Variable_Name,Value_Code),
    rsegmenter::row_tab %>% 
      dplyr::arrange(Variable_Name,Value_Code))
})

testthat::test_that("Col index profile tables",{
  testthat::expect_equal(
    rsegmenter::test_seg_unlabelled %>% 
      dplyr::mutate(rsegmenter::factor_segmentation(rsegmenter::test_seg_unlabelled,
                                                    vars = c("seg1","seg2","seg3","seg4",
                                                             "seg5","seg6","seg7","seg8",
                                                             "seg9","seg10"),
                                                    weight_var = "weight",
                                                    num_sols = c(5),
                                                    fac_assign = "avg_loading") %>%
                      rsegmenter::extract_factor_segments()) %>% 
      rsegmenter::profile_table_col_index(category_vars = c("seg1","seg2","seg3","seg4",
                                                            "seg5","seg6","seg7","seg8",
                                                            "seg9","seg10"),
                                          numeric_vars = NULL,
                                          banner_var = c("Factor_Cluster_Soln_5")) %>% 
      dplyr::arrange(Variable_Name,Value_Code),
    rsegmenter::col_ind_tab %>% 
      dplyr::arrange(Variable_Name,Value_Code))
})

