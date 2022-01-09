#' lts_leadLagClusterPlot
#'
#'@importFrom ComplexHeatmap Heatmap
#'@importFrom circlize colorRamp2
#'@importFrom dplyr ungroup
#'@param .lts_clusterOutput_LAGranges output from
#'lifeTimesChain()
#'@param .removeInstanceOfCategoricalByName name a feature that should be removed from
#'clustering (eg. because of perfect correlation, high noise, or artefact)
#'@param .lts_variables user mapping of variables to function
#'@param .categoryToRemoveInstanceFrom factor type, to remove factor level from
#'
#'@export
#'
#'


#Function to cluster contitions (eg. treatments) and variables (eg. feature measurements)
#by difference in past vs future lags

###Toggle these next two lines on and off during testing
# leadLagPlotInput
# join_medianDiff_meanLagRange_outputCCFdata_withMetaData


lts_leadLagClusterPlot <- function(.lts_clusterOutput_LAGranges = lts_clusterOutput_LAGranges,
                               .lts_variables = lts_variables,
                               .removeInstanceOfCategoricalByName = NULL,
                               .categoryToRemoveInstanceFrom = NULL){
  #
  #
  # subset_meanLagRange_join_outputCCFdata <-
  # join_medianDiff_meanLagRange_outputCCFdata_withMetaData[join_medianDiff_meanLagRange_outputCCFdata_withMetaData$lagRange != "zeroLAG" ,]
  #
  # if(!missing(removeFeatureByname)) {
  # #   subset_meanLagRange_join_outputCCFdata <-  subset_meanLagRange_join_outputCCFdata %>%
  # #       filter(!grepl(paste0(removeByname, collapse = "|"), an_CCF_Feature))}
  # .lts_clusterOutput_LAGranges = lts_clusterOutput_LAGranges
  # .lts_variables = lts_variables
  # .removeInstanceOfCategoricalByName = NULL
  # .categoryToRemoveInstanceFrom = NULL #eg. from .lts_variables$compare_by

  lts_final_clusters <-.lts_clusterOutput_LAGranges$medDiff_meanLag_lts_clusterCCFs #shorten list path to data

  sub_lts_final_clusters <- lts_final_clusters[lts_final_clusters$lagRange != "zeroLAG",] #subset to remove zero lag rows

  if(!is.null(.removeInstanceOfCategoricalByName)) { #if statement to remove categorical variables by name
    sub_lts_final_clusters <-  sub_lts_final_clusters %>%
      dplyr::filter(!grepl(.removeInstanceOfCategoricalByName, .categoryToRemoveInstanceFrom))
  }else {


    # join_meanLagRange_join_outputCCFdata <- join_medianDiff_meanLagRange_outputCCFdata_withMetaData

    #subset to remove zero lag

    #remove feature by name (helps for eliminating NA features etc)


    #remove feature if has NA

    toRemove <-  subset_meanLagRange_join_outputCCFdata %>%
      dplyr::filter(is.na(medianPrePostPerTF)) # NB: could remove this step

    if(nrow(toRemove)>0){
      print(paste("There are...",nrow(toRemove),"observations with NA. Removing...",toRemove$key_num))
    }

    rmna_subset_meanLagRange_join_outputCCFdata <-  subset_meanLagRange_join_outputCCFdata %>%
      dplyr::filter(!is.na(medianPrePostPerTF)) # NB: could remove this step

    # View(rmna_diffIn_Pre_vs_Postcorrelation)
    # head(rmna_diffIn_Pre_vs_Postcorrelation)
    # colnames(rmna_diffIn_Pre_vs_Postcorrelation)
    # unique(diffIn_Pre_vs_Postcorrelation$an_CCF_Feature)

    filt_rmna_subset_meanLagRange_join_outputCCFdata<- rmna_subset_meanLagRange_join_outputCCFdata %>%
      dplyr::ungroup()%>%
      dplyr::select(lts_variables$lts_compare_by,medianPrePostPerTF)

    unq_filt_rmna_subset_meanLagRange_join_outputCCFdata <- unique(filt_rmna_subset_meanLagRange_join_outputCCFdata) #filtering before make matrix
    # filteredForM_diffIn_Pre_vs_Postcorrelation
    # dim(filteredForM_diffIn_Pre_vs_Postcorrelation)


    wide_unq_filt_rmna_subset_meanLagRange_join_outputCCFdata <- tidyr::pivot_wider(
      unq_filt_rmna_subset_meanLagRange_join_outputCCFdata,
      names_from = lts_variables$lts_compare_by[1],
      values_from = medianPrePostPerTF)
    mw_prePost <- as.matrix(wide_unq_filt_rmna_subset_meanLagRange_join_outputCCFdata[-1])
    rownames(mw_prePost) <- wide_unq_filt_rmna_subset_meanLagRange_join_outputCCFdata[[1]]

    # https://stackoverflow.com/questions/9505849/r-how-to-get-row-and-column-names-of-the-true-elements-of-a-matrix
    hasNA_So_removedFromMatrix <- names(which(rowSums(is.na(mw_prePost)) > 0))
    print(paste("Warning:...",hasNA_So_removedFromMatrix,"...had NA, so removed from matrix"))

    narm_mw_prePost <- na.omit(mw_prePost)


    # library(ComplexHeatmap)
    col_fun = circlize::colorRamp2(c(min(narm_mw_prePost), 0,max(narm_mw_prePost)), c("steelblue4", "white", "gold"))
    col_fun(seq(-10, 10))

    # ComplexHeatmap::Heatmap(narm_mw_prePost, col = col_fun)
    return((Heatmap(narm_mw_prePost, col = col_fun)))
  }
}



