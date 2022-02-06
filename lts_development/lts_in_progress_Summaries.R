#lts_make_ccf_summary_stats

lts_ccf_summary_stats <- function(.lts_ccfWithMetaData = lts_ccfWithMetaData_compareBy,
                                  .lts_variables = NULL,
                                  .chosenLAGforClustering = modeMaxCorrLAG){

}

if(.lts_variables$lts_plot_measured_variables == TRUE){ #include "theFeature" in lts_compare_by, for plotting
  .lts_variables$lts_compare_by <- c(.lts_variables$lts_compare_by, "theFeature")
} #Added this to have compare by "theFeature",



if(is.null(.lts_variables)){
  .lts_variables <- lts_defaultVariables
}

.lts_compare_by <- .lts_variables$lts_compare_by

#ONE: Group by categorical variables and get mean for each lag

#groups by categorical variaables and LAGs and gets mean correlation for each lag
meanCorrPerLag <- .lts_ccfWithMetaData %>%
  dplyr::group_by(!!rlang::sym(.lts_compare_by[[1]]),
                  !!rlang::sym(.lts_compare_by[[2]]),
                  theLAG) %>% #group by vector of cluster groups
  dplyr::summarise(meanCorrPerLag = mean(theCCF, na.rm = TRUE)) #summarise the mean at lag 0

join_ccflist_wMetaD <- dplyr::left_join(
  .lts_ccfWithMetaData, meanCorrPerLag,
  by = c(.lts_compare_by,
         names(.lts_ccfWithMetaData[grepl("theLAG",names(.lts_ccfWithMetaData))]) #select column containing "theLAG"
  )) # join mean lag to input dataframe for clustering


#TODO --> Make this a final step #Remove identical at zero lag

#remove features that are identical at zero lag #Todo, remove unused factor levels eg. coords
identicalMeasuresAt0 <- join_ccflist_wMetaD[join_ccflist_wMetaD$meanCorrPerLag ==1, ] #define identical measures
unq_identicalFeatures <- unique(identicalMeasuresAt0[,"theFeature"]) #added coma to subset the column and give a vector, not dataframe
if(nrow(identicalMeasuresAt0) > 0){print(paste("Warning: feature ",unq_identicalFeatures,"has correlation 1 at lag zero and will be removed"))
  join_ccflist_wMetaD <- join_ccflist_wMetaD[!grepl(paste(unq_identicalFeatures, collapse="|"), join_ccflist_wMetaD$theFeature),]
} #remove features that are perfectly correlated

#TWO: Get lag with max correlation in the entire dataset

#get Lag with max correlation in the entire dataset
maxCorrLAG_total <-
  unique(
    join_ccflist_wMetaD[ #look in ccf with mean and metadata
      join_ccflist_wMetaD$meanCorrPerLag == max(join_ccflist_wMetaD$meanCorrPerLag), #get row where max meanLAG
      "theLAG"]) # in column "theLAG

#define mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#THREE get lag with most frequently occuring max correlation in the dataset
#group by categorical variables

#get Lag with the mode max correlation
modeMaxCorrLAG <- join_ccflist_wMetaD %>%
  dplyr::select(.lts_variables$lts_compare_by, theLAG, meanCorrPerLag)%>%
  dplyr::group_by(dplyr::across(.lts_variables$lts_compare_by)) %>%
  dplyr::top_n(1, meanCorrPerLag) %>%
  unique() %>%
  dplyr::pull(theLAG) %>%
  Mode()

#FOUR get lag with max correlation per time track

#FIVe get lag with most frequently occuring max correlation per categorical variables


#six calculate average correlation at the lag with mode max correlation

df_meanCorrAtModeMaxLAG <- join_ccflist_wMetaD %>%  #calculate mean Corr at Lag with mode max correlation
  dplyr::filter(theLAG == modeMaxCorrLAG) %>%
  dplyr::group_by(dplyr::across(.lts_variables$lts_compare_by)) %>% #group by vector of cluster groups
  dplyr::summarise(meanCorrAtModeMaxLAG = mean(theCCF, na.rm = TRUE)) #summarise the mean at lag 0

join_ccflist_wMetaD_mode <- dplyr::left_join(
  join_ccflist_wMetaD,
  df_meanCorrAtModeMaxLAG,
  by = c(.lts_compare_by) #no longer need to select column containing "theLAG"
)

#
# #create matrix of values using compare_by and chosen lag
# #This step reduces everything to one lag value, which will allow for clustering
# m_wide_join_ccflist_wMetaD <- join_ccflist_wMetaD_mode %>% #make wider, put treatment as colnames, put values as lag0
#   dplyr::filter(theLAG == .chosenLAGforClustering)%>% # choose lag that is mode most correlated lag
#   dplyr::select(.lts_variables$lts_compare_by, meanCorrPerLag)%>%
#   unique()%>%
#   tidyr::pivot_wider(
#     id_col = c("meanCorrPerLag",.lts_variables$lts_compare_by),
#     names_from = .lts_variables$lts_compare_by[1], #lts_cluster_feature1 hereafter
#     values_from = "meanCorrPerLag")
#
# mCCF_chosenLAG <- as.matrix(m_wide_join_ccflist_wMetaD[-1]) #make a numerical only matrix of mean correlation at lag zero, by removing first column
# rownames(mCCF_chosenLAG) <-  m_wide_join_ccflist_wMetaD[[1]] # add rownames to matrix (#lts_cluster_feature2)
#
# #cluster matrix
# lts_hclustColumn_order_feature1 <- hclust(dist(t(mCCF_chosenLAG)))$order # get column order from clustered matrix and set this as a variable #BROKEN HERE
# lts_hclustColumn_LABELS_feature1 <-hclust(dist(t(mCCF_chosenLAG)))$labels
# lts_hclustRow_order_feature2 <- hclust(dist(mCCF_chosenLAG))$order # get row order from clustered matrix and set this as a  variable
# lts_hclustColumn_LABELS_feature2 <-hclust(dist(mCCF_chosenLAG))$labels
#
# mCCF_chosenLAG[lts_hclustRow_order_feature2, lts_hclustColumn_order_feature1] #display matrix organised by rows and columns
#
# # get list of rows with names for ordering (order the factor this way)
# #original column ordering from matrix
# column_feature1 <- lts_hclustColumn_LABELS_feature1
# column_feature1
#
# #original row ordering from matrix
# row_feature2 <- lts_hclustColumn_LABELS_feature2
# row_feature2
#
# #reorder columns by clustering
# clust_column_feature1 <- column_feature1[lts_hclustColumn_order_feature1] #make a new or desired feature order based on the row order
# clust_column_feature1
#
# #reorder rows by clustering
# clust_row_feature2 <- row_feature2[lts_hclustRow_order_feature2] #make a new or desired feature order based on the row order
# clust_row_feature2
#
# #update main dataframe (update the factor levels for Treatments and Features, to be based on clustering)
# # reload ->join_ccflist_wMetaD #DELETE Th
#
# # organise cluster column levels
# join_ccflist_wMetaD_mode[.lts_variables$lts_compare_by[[1]] ] <- as.factor(join_ccflist_wMetaD_mode[,.lts_variables$lts_compare_by[[1]] ]) #make feature1 a factor
# join_ccflist_wMetaD_mode[.lts_variables$lts_compare_by[[1]] ] <- factor(join_ccflist_wMetaD_mode[,.lts_variables$lts_compare_by[[1]] ], levels =clust_column_feature1) #
# levels(join_ccflist_wMetaD_mode[,.lts_variables$lts_compare_by[[1]] ]) #check ordering
#
# # organise cluster row levels
# join_ccflist_wMetaD_mode[.lts_variables$lts_compare_by[[2]] ] <- as.factor(join_ccflist_wMetaD_mode[,.lts_variables$lts_compare_by[[2]] ]) #make feature1 a factor
# join_ccflist_wMetaD_mode[.lts_variables$lts_compare_by[[2]] ] <- factor(join_ccflist_wMetaD_mode[,.lts_variables$lts_compare_by[[2]] ], levels =clust_row_feature2) #
# levels(join_ccflist_wMetaD_mode[,.lts_variables$lts_compare_by[[2]] ]) #check ordering
#
# lts_clustered_ccflist <- join_ccflist_wMetaD_mode
# lts_mCCF_chosenLAG <- mCCF_chosenLAG
#
# lts_clusterOutput <- list(lts_clustered_ccflist = lts_clustered_ccflist,
#                           lts_mCCF_chosenLAG = lts_mCCF_chosenLAG,
#                           modeMaxCorrLAG = modeMaxCorrLAG) #this list output includes the matrix of values (mCCF) at chosen lag for dendrogram creation in plotting steps, an alternative is to generate this dendrogram denovo in a separate function or in the plotting step.
#
# # TODO: Try and use this as output
# # lts_clusterOutput <- list(list(lts_clustered_ccflist = lts_clustered_ccflist),
# #                           list(lts_mCCF_chosenLAG = lts_mCCF_chosenLAG),
# #                           list(modeMaxCorrLAG = modeMaxCorrLAG)) #this list output includes the matrix of values (mCCF) at chosen lag for dendrogram creation in plotting
#




#section on summaries of differences


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










return(lts_clusterOutput)
}
