#' lts_leadLagClusterPlot
#'
#'Function to cluster contitions (eg. treatments) and variables (eg. feature measurements) by difference
#'in past vs future lags
#'
#'@importFrom ComplexHeatmap Heatmap
#'@importFrom circlize colorRamp2
#'@importFrom dplyr ungroup %>%
#'@importFrom magrittr %>%
#'@import magrittr
#'@param .removeInstanceOfCategoricalByName name a feature that should be removed from
#'clustering (eg. because of perfect correlation, high noise, or artefact)
#'@param .lts_output results of lts_input() function, includes
#'cross correlation calculations and user input variables
#'@param .categoryToRemoveInstanceFrom factor type, to remove factor level from
#'
#'@export
#'

lts_leadLagClusterPlot <- function(.lts_output = NULL,
                                   .removeInstanceOfCategoricalByName = NULL,
                                   .categoryToRemoveInstanceFrom = NULL){

  if(is.null(.lts_output)){
    return(print("please enter some lifeTimes output"))
  }
  subset_sum_join_outputCCFdata <-  .lts_output$lts_CCFcalcs

  lts_final_clusters <-.lts_output$lts_CCFcalcs #shorten list path to data

  sub_lts_final_clusters <- lts_final_clusters[lts_final_clusters$lagRange != "zeroLAG",] #subset to remove zero lag rows

  if(!is.null(.removeInstanceOfCategoricalByName)) { #if statement to remove categorical variables by name
    sub_lts_final_clusters <-  sub_lts_final_clusters %>%
      dplyr::filter(!grepl(.removeInstanceOfCategoricalByName, .categoryToRemoveInstanceFrom))
  }else {

    toRemove <-  sub_lts_final_clusters %>%
      dplyr::filter(is.na(medianPrePostPerTF)) # NB: could remove this step

    if(nrow(toRemove)>0){
      print(paste("There are...",nrow(toRemove),"observations with NA. Removing...",toRemove$lts_uniqueID_colname)) #fix here to make uniqueID_colname instead of "key_num"
    }

    rmna_subset_meanLagRange_join_outputCCFdata <-  sub_lts_final_clusters %>%
      dplyr::filter(!is.na(medianPrePostPerTF)) # NB: could remove this step

    filt_rmna_subset_meanLagRange_join_outputCCFdata<- rmna_subset_meanLagRange_join_outputCCFdata %>%
      dplyr::ungroup()%>%
      dplyr::select(.lts_output$lts_variables$lts_compare_by,medianPrePostPerTF)

    unq_filt_rmna_subset_meanLagRange_join_outputCCFdata <- unique(filt_rmna_subset_meanLagRange_join_outputCCFdata) #filtering before make matrix

    wide_unq_filt_rmna_subset_meanLagRange_join_outputCCFdata <- tidyr::pivot_wider(
      unq_filt_rmna_subset_meanLagRange_join_outputCCFdata,
      names_from = .lts_output$lts_variables$lts_compare_by[1],
      values_from = medianPrePostPerTF)
    mw_prePost <- as.matrix(wide_unq_filt_rmna_subset_meanLagRange_join_outputCCFdata[-1])
    rownames(mw_prePost) <- wide_unq_filt_rmna_subset_meanLagRange_join_outputCCFdata[[1]]

    hasNA_So_removedFromMatrix <- names(which(rowSums(is.na(mw_prePost)) > 0))
    print(paste("Warning:...",hasNA_So_removedFromMatrix,"...had NA, so removed from matrix"))

    narm_mw_prePost <- na.omit(mw_prePost)

    col_fun = circlize::colorRamp2(c(min(narm_mw_prePost), 0,max(narm_mw_prePost)), c("steelblue4", "white", "gold"))
    col_fun(seq(-10, 10))

    return((Heatmap(narm_mw_prePost, col = col_fun)))
  }
}



