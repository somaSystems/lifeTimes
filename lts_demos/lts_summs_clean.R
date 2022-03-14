#' lts_summs_clean
#'function to take summary stats from lts and widen to make "tidy" so that there is a single row per observation
#'function also generates summary stats for original data
#'outputs original_summs, ccf_summs, and joined_summs which are a join of original and ccf summaries
#'
#' @importFrom tidyr drop_na
#' @importFrom tidyr pivot_wider
#'
#' @importFrom magrittr %>%
#' @param .lts_output output from the lifeTimes main workflow
#'
#' "plt_dendr","heatmapLagZero","rawTraces","clusteredLines")
#'
#' @return original_summs, ccf_summs, and joined_summs which are a join of original and ccf summaries
#'
#' @export

#0. take workflow so far
#1. summarise original.
#2. summarise new
#3. join original and new

lts_summs_clean <- function(.lts_output = NULL){

  #define mode function
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }

  #define internal function variables
.lts_measures <- unlist(.lts_output$lts_variables$lts_pariedComparisons)
.lts_unique_ID_colnames <- .lts_output$lts_variables$lts_uniqueID_colname
.lts_compare_by <- .lts_output$lts_variables$lts_compare_by
# .lts_measures <- unlist(.lts_output$lts_variables$lts_pariedComparisons)
names(.lts_measures) <- .lts_measures #fix names to be the same as the variables

  #summarise original input data
  sumOriginal <- .lts_output$lts_variables$lts_data %>%
    # dplyr::select(key_num, season, catchmentRegion) %>%
    dplyr::select(!!!rlang::syms(c(.lts_compare_by, .lts_unique_ID_colnames, .lts_measures))) %>%
    dplyr::group_by(!!!rlang::syms(c(.lts_unique_ID_colnames,.lts_compare_by)))%>% #keep categoricals, although not of interest for summary, the metadata is important
    dplyr::summarise_at(c(.lts_measures), c(mean = mean,
                                            median = median,
                                            sd = sd,
                                            var = var,
                                            IQR = IQR,
                                            # quantile = quantile,
                                            min = min,
                                            max = max,
                                            sum = sum))

  #summarise ccf from original data
  singleSums <- .lts_output$lts_ccf_summaries$lts_singleton_summ_metadata

  #get categorical vectors
  .lts_compare_by <- as.vector(.lts_output$lts_variables$lts_compare_by)

  #make a function
  '%!in%' <- function(x,y)!('%in%'(x,y))

  #get index of columns that are in single sums but are not the categorical or unique IDs
  valsFrom <- which(!(colnames(singleSums) %in% c(.lts_compare_by,.lts_unique_ID_colnames))) #colnames that are not already in categotical or uniqur ID

  #make single sums wider, using valsFrom, so that only one row per key num
  w_ccf_singleSumms <- pivot_wider(singleSums, #make wider so one row per kry num
                              names_from = lag_range,
                              values_from = all_of(valsFrom),
                              names_glue = "{lag_range}_{.value}")


  #now join sumOriginal and w_ccf_singleSums
  names.use <- names(w_ccf_singleSumms)[!(names(w_ccf_singleSumms) %in% .lts_compare_by)]

  w_ccf_singleSumms_noCategories <- w_ccf_singleSumms[, names.use ]

  join_original_ccf_summ <- merge(sumOriginal,w_ccf_singleSumms_noCategories, by = c(.lts_unique_ID_colnames))

# ?append()



  # lts_cleaned_summs <- c(
  #   list( lts_variables = .lts_variables),
  #   .lts_ccf_with_summs, #this is already two named lsits so pasted in
  #   list(lts_clust_outputs = list( clust_matrix = mCCF_chosenLAG,
  #                                  clust_column_feature1 = clust_column_feature1, #the orders of categorical variables
  #                                  clust_row_feature2 = clust_row_feature2)
  #   ),
  #   list(lts_clust_ccfs_with_meta = .lts_ccf_with_meta), #the ccfs with factor levels ordered by clustering,

  lts_cleaned_summs_list <- list(lts_clean_summs =list(

      lts_clean_summ_original = sumOriginal,
      # lts_singleton_summ_modeMax = lts_singleton_summ_modeMax, #recent added 12_2_2022
      lts_clean_summ_ccf = w_ccf_singleSumms,
      lts_clean_summ_join = join_original_ccf_summ #recent added 12_2_2022
    )
    )


  lts_clean_summs <- append(.lts_output, lts_cleaned_summs_list)

      return(lts_clean_summs)
  #   ))
  # )

}
