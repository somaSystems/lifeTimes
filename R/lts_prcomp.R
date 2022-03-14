#' lts_prcomp
#'
#' @importFrom tidyr drop_na
#'
#' @param .lts_summary_data output Summary data from the lifeTimes main workflow
#'
#'
#' "plt_dendr","heatmapLagZero","rawTraces","clusteredLines")
#'
#' @return a plot of features and treatments clustered by CCF at lag zero
#'
#' @export
#'

lts_prcomp <- function(.lts_summary_data = NULL){
  #For these three dataframes
  #original, ccf and merge
  #look at PCA space

  # .lts_summary_data = .lts_summary_data$lts_ccf_summaries$lts_singleton_summ_metadata

  #remove columns which are all NA
  colnarm_w_singleSums <- .lts_summary_data[,colSums(is.na(.lts_summary_data))<nrow(.lts_summary_data)] #remove na cols

  rownarm_colnarm_w_singleSums <- colnarm_w_singleSums %>% #remove rows where some are na
    tidyr::drop_na()

  lts_all_narm <- rownarm_colnarm_w_singleSums #cleaned numeric and labels with no NA

  num_cols <- unlist(lapply(rownarm_colnarm_w_singleSums, is.numeric))         # Identify numeric columns

  # label_cols <- unlist(lapply(rownarm_colnarm_w_singleSums, is.numeric))

  num_w_singleSums <- rownarm_colnarm_w_singleSums[ , num_cols]                        # Subset numeric columns of data

  lts_noVar <- as.numeric(which(apply(num_w_singleSums, 2, var) ==0)) #find columns without variance
  if(length(lts_noVar) >0){    #remove columns without variance
    hasvar_num_w_singleSums <- num_w_singleSums[ -lts_noVar]} else{
      hasvar_num_w_singleSums <- num_w_singleSums
    }


  label_w_singleSums <- rownarm_colnarm_w_singleSums[ , !num_cols] #only get labels after NA rows are removed
  # label_w_singleSums$combinedCategoricals <- paste0("combine_",.lts_compare_by[[1]],"_",.lts_compare_by[[2]])

  # num_w_singleSums <- subset(rownarm_colnarm_w_singleSums, is.numeric(rownarm_colnarm_w_singleSums))
  pc_num_w_singleSums <- prcomp(hasvar_num_w_singleSums, scale. = TRUE, center = TRUE)

  lts_pc_results <- list(lts_pc_values = pc_num_w_singleSums,
                         lts_labels_pc = label_w_singleSums,
                         lts_values_pc = hasvar_num_w_singleSums,
                         lts_all_pc = lts_all_narm
  )
  return(lts_pc_results)
}
