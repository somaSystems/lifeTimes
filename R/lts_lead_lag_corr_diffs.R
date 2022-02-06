#' lts_lead_lag_corr_diffs
#'
#' @param .lts_clusterOutput output from lts_get_summary_ccf_stats()
#' @param .lts_variables output from lts_input()
#'
#' @return
#'
lts_leadLagCorr_diffs <- lts_leadLagCorr_diffs <- function(
  .lts_clusterOutput = lts_clusterOutput,
  .lts_variables = NULL)
{


  if(is.null(.lts_variables)){
    .lts_variables <- lts_defaultVariables
  }

  if(.lts_variables$lts_plot_measured_variables == TRUE){ #include "theFeature" in lts_compare_by, for plotting
    #make new column for compare by
    .lts_variables$lts_compare_by <- c(.lts_variables$lts_compare_by, "lts_theFeature") #new compare by variable
    .lts_clusterOutput$lts_clustered_ccflist$lts_theFeature <- .lts_clusterOutput$lts_clustered_ccflist$theFeature
  } ##Added this to have compare by "theFeature", hotfix

  .clusteredByChosenLAG <-  .lts_clusterOutput$lts_clustered_ccflist

  lts_categoricalVariables <- c(.lts_variables$lts_compare_by[[1]],.lts_variables$lts_compare_by[[2]])

  # function to calculate median difference (asymmetry) in the mean of lead and lagging time points for different variables
  #TODO, make summaries of pre and post lags medians, rather than means

  #arrange data
  Arranged_lts_clusterCCFs <- .clusteredByChosenLAG %>% #arrange input dataframe
    dplyr::arrange(!!rlang::sym(.lts_variables$lts_uniqueID_colname), theFeature, #kmfix here
                   !!rlang::sym(lts_categoricalVariables[[1]]),
                   !!rlang::sym(lts_categoricalVariables[[2]]),
                   theLAG)%>% #arranging by LAG here is important
    dplyr::group_by(!!rlang::sym(.lts_variables$lts_uniqueID_colname), theFeature, #kmfix here
                    !!rlang::sym(lts_categoricalVariables[[1]]),
                    !!rlang::sym(lts_categoricalVariables[[2]]))

  LAGmin <- min(Arranged_lts_clusterCCFs$theLAG) #define LAGmin
  LAGmax <- max(Arranged_lts_clusterCCFs$theLAG) #define LAGmax

  #create new variable of lag range,
  Arranged_lts_clusterCCFs$lagRange <- # nested ifelse to set ranges of lags
    ifelse(Arranged_lts_clusterCCFs$theLAG >LAGmin & Arranged_lts_clusterCCFs$theLAG <0 , "negativeLAG",
           ifelse(Arranged_lts_clusterCCFs$theLAG >0 & Arranged_lts_clusterCCFs$theLAG <LAGmax, "positiveLAG","zeroLAG"))

  #get mean for lags before and after zero
  #first arrange by LAG, then calculate
  meanLagRange_lts_clusterCCFs <- Arranged_lts_clusterCCFs %>% # Organise by Cell ID, Feature, Treatment, and Lag (timepoint),
    dplyr::arrange(!!rlang::sym(.lts_variables$lts_uniqueID_colname),theFeature, #kmfix here
                   !!rlang::sym(lts_categoricalVariables[[1]]),
                   !!rlang::sym(lts_categoricalVariables[[2]]),
                   theLAG)%>%
    dplyr::group_by(!!rlang::sym(.lts_variables$lts_uniqueID_colname), theFeature, #kmfix here
                    !!rlang::sym(lts_categoricalVariables[[1]]),
                    !!rlang::sym(lts_categoricalVariables[[2]]),
                    lagRange) %>%
    dplyr::summarise(meanCorrforLAGrange = mean(theCCF, na.rm = TRUE)) # calculate average correlations in lags (either side of zero)

  #remove common names before joining, get vector of non common names from second dataframe and subset before joining
  varList<- names(meanLagRange_lts_clusterCCFs)[!(names(meanLagRange_lts_clusterCCFs) %in% names(Arranged_lts_clusterCCFs))] # get non common names
  varList

  #comment out in JAN
  # #join mean CorrForLAGrange to Arranged_lts_clusterCCFs
  # Arranged_lts_clusterCCFs <- dplyr::left_join(Arranged_lts_clusterCCFs,
  #                                              meanLagRange_lts_clusterCCFs[c(varList, .lts_variables$lts_uniqueID_colname)], by = .lts_variables$lts_uniqueID_colname)


  #added in JAN
  jArranged_lts_clusterCCFs <- dplyr::left_join(Arranged_lts_clusterCCFs,
                                                meanLagRange_lts_clusterCCFs[c(varList, #unique new column?
                                                                               "theFeature",##hotfixJan2022
                                                                               "lagRange",##hotfixJan2022
                                                                               .lts_variables$lts_uniqueID_colname)],
                                                by = c(.lts_variables$lts_uniqueID_colname,"theFeature","lagRange")) ##hotfixJan2022 so that join by Feature, and lagRange
  #also make "jArrange the name of variable".

  #make wider, taking names from lag range and values from mean lag
  #This gives a table of every, object (key_num), with average prior, post and zero lags calculated
  wider_meanLagRange_lts_clusterCCFs <- meanLagRange_lts_clusterCCFs %>%
    tidyr::pivot_wider(
      id_cols = c(!!rlang::sym(.lts_variables$lts_uniqueID_colname), theFeature, meanCorrforLAGrange,
                  !!rlang::sym(lts_categoricalVariables[[1]]),
                  !!rlang::sym(lts_categoricalVariables[[2]]),
      ),
      names_from = "lagRange",
      values_from = "meanCorrforLAGrange")
  wider_meanLagRange_lts_clusterCCFs

  wider_meanLagRange_lts_clusterCCFs$meanPrior_meanPost_diff <- #calculate difference in negative(prior) and positive(post) time lags
    wider_meanLagRange_lts_clusterCCFs$negativeLAG -
    wider_meanLagRange_lts_clusterCCFs$positiveLAG


  #remove common names before joining, get vector of non common names from second dataframe and subset before joining
  varList<- names(wider_meanLagRange_lts_clusterCCFs)[!(names(wider_meanLagRange_lts_clusterCCFs) %in% names(jArranged_lts_clusterCCFs))] # get non common names


  #Added in Jan
  #join the difference between prior and post lags to the arranged clusters
  join_meanLagRange_jArranged_lts_clusterCCFs <-
    dplyr::left_join( #join prior to post ratio back to dataframe
      jArranged_lts_clusterCCFs,
      wider_meanLagRange_lts_clusterCCFs[c(varList,.lts_variables$lts_uniqueID_colname, #kmfix here #subset second dataframe to keep only: non common, and join by variables
                                           "theFeature")],
      # lts_categoricalVariables[[1]],
      # lts_categoricalVariables[[2]])], #hotfixJan2022 removed these as not needed
      by = c(.lts_variables$lts_uniqueID_colname, #kmfix here
             "theFeature"))
  # lts_categoricalVariables[[1]],
  # lts_categoricalVariables[[2]])) #hotfixJan2022 removed these as not needed

  #commented out in JAN
  # join_meanLagRange_jArranged_lts_clusterCCFs <- dplyr::left_join( #join prior to post ratio back to dataframe
  #   jArranged_lts_clusterCCFs,
  #   wider_meanLagRange_lts_clusterCCFs[c(varList,.lts_variables$lts_uniqueID_colname, #kmfix here #subset second dataframe to keep only: non common, and join by variables
  #                                        lts_categoricalVariables[[1]],
  #                                        lts_categoricalVariables[[2]])],
  #   by = c(.lts_variables$lts_uniqueID_colname, #kmfix here
  #          lts_categoricalVariables[[1]],
  #          lts_categoricalVariables[[2]]))

  #check dimensions are the same
  dim(jArranged_lts_clusterCCFs) # wider_meanLagRange_join_outputCCFdata
  dim(join_meanLagRange_jArranged_lts_clusterCCFs)


  #Added in Jan
  # remove common names before joining
  summaryOfMedianDifferencePriorAndPost <- #get overall feature mean for clustering
    join_meanLagRange_jArranged_lts_clusterCCFs %>%
    dplyr::group_by(!!rlang::sym(lts_categoricalVariables[[1]]),
                    !!rlang::sym(lts_categoricalVariables[[2]]),
                    theFeature) %>% #hotfixJan2022
    dplyr::summarise(medianPrePostPerTF = median(meanPrior_meanPost_diff))

  # summaryOfMedianDifferencePriorAndPost
  # join_meanLagRange_Arranged_lts_clusterCCFs

  join_medianDiff_meanLagRange_jArranged_lts_clusterCCFs <- #Join summary back to original dataframe, make a global variable
    dplyr::left_join(join_meanLagRange_jArranged_lts_clusterCCFs,
                     summaryOfMedianDifferencePriorAndPost[],
                     by = c(lts_categoricalVariables[[1]],
                            lts_categoricalVariables[[2]],
                            "theFeature")) #hotfixJan2022

  #commented out in JAN
  # # remove common names before joining
  # summaryOfMedianDifferencePriorAndPost <- #get overall feature mean for clustering
  #   join_meanLagRange_jArranged_lts_clusterCCFs %>%
  #   dplyr::group_by(!!rlang::sym(lts_categoricalVariables[[1]]),
  #                   !!rlang::sym(lts_categoricalVariables[[2]])) %>%
  #   dplyr::summarise(medianPrePostPerTF = median(meanPrior_meanPost_diff))
  #
  #
  # join_medianDiff_meanLagRange_jArranged_lts_clusterCCFs <- #Join summary back to original dataframe, make a global variable
  #   dplyr::left_join(join_meanLagRange_jArranged_lts_clusterCCFs,
  #                    summaryOfMedianDifferencePriorAndPost[],
  #                    by = c(lts_categoricalVariables[[1]],
  #                           lts_categoricalVariables[[2]]))

  medDiff_meanLag_lts_clusterCCFs <- as.data.frame(join_medianDiff_meanLagRange_jArranged_lts_clusterCCFs)

  #consider appending to input list and returning appended list
  lts_box <- c(list(lts_CCFcalcs = medDiff_meanLag_lts_clusterCCFs),
               list(lts_rawCCFout = .lts_clusterOutput),
               list(lts_variables = .lts_variables))

  return(lts_box)
}
