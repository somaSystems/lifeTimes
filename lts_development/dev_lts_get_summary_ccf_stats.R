#' lts_get_summary_ccf_stats
#'
#' @param .lts_ccfWithMetaData out put from lts_join_metadata_to_ccf
#' @param .lts_variables output from lts_input()
#' @param .chosenLAGforClustering a chosen lag for clustering summary statistics
#'
#' @return
#'


#This function will take data
#and perform clustering for ggplot superlot of ccfs
#the data are categorical variables
#the clustering can be by
# default, correlation at the mode max lag per variable
# eg. find a variables most common max lag, and cluster data by this
# OR cluster by correlation at max lag in the dataset

#THIS FUNCTION IS FOR PLOTTING GROUPED BY CAT GROUPS (single obs are drawn as lines)
#ANOTHER FUNCTION WILL BE USED FOR PLOTTING UNGROUPED DATA

# lts_clusterCCFs <-function(
#   .lts_ccfWithMetaData = lts_ccfWithMetaData_compareBy,
#   .lts_variables = NULL,
#   .chosenLAGforClustering = modeMaxCorrLAG){


.lts_ccfWithMetaData = lts_sum_ccf
.lts_variables = lts
.chosenLAGforClustering = 1


  if(.lts_variables$lts_plot_measured_variables == TRUE){ #include "theFeature" in lts_compare_by, for plotting
    .lts_variables$lts_compare_by <- c(.lts_variables$lts_compare_by, "theFeature")
  } #Added this to have compare by "theFeature",

  if(is.null(.lts_variables)){
    .lts_variables <- lts_defaultVariables
  }

  .lts_compare_by <- .lts_variables$lts_compare_by

 #create matrix of values using compare_by and chosen lag
  #This step reduces everything to one value per set of categorical variables (representing many unique observations),
  # this is the value that will be used for clustering

  #create matrix of values using compare_by and chosen lag
  #This step reduces everything to one lag value, which will allow for clustering

  lts_sum_ccf$lts_catGroups_modeMaxCorrLAG



  m_wide_lts_catGroups_modeMaxCorrLAG <- lts_sum_ccf$lts_catGroups_modeMaxCorrLAG %>%
    dplyr::group_by(!!rlang::sym(.lts_compare_by[[1]]),
                    !!rlang::sym(.lts_compare_by[[2]])) %>%

  m_wide_join_ccflist_wMetaD <- join_ccflist_wMetaD_mode %>% #make wider, put treatment as colnames, put values as lag0
    dplyr::filter(theLAG == .chosenLAGforClustering)%>% # choose lag that is mode most correlated lag
    dplyr::select(.lts_variables$lts_compare_by, meanCorrPerLag)%>%
    unique()%>%
    tidyr::pivot_wider(
      id_col = c("meanCorrPerLag",.lts_variables$lts_compare_by),
      names_from = .lts_variables$lts_compare_by[1], #lts_cluster_feature1 hereafter
      values_from = "meanCorrPerLag")

  mCCF_chosenLAG <- as.matrix(m_wide_join_ccflist_wMetaD[-1]) #make a numerical only matrix of mean correlation at lag zero, by removing first column
  rownames(mCCF_chosenLAG) <-  m_wide_join_ccflist_wMetaD[[1]] # add rownames to matrix (#lts_cluster_feature2)

  #cluster matrix
  lts_hclustColumn_order_feature1 <- hclust(dist(t(mCCF_chosenLAG)))$order # get column order from clustered matrix and set this as a variable #BROKEN HERE
  lts_hclustColumn_LABELS_feature1 <-hclust(dist(t(mCCF_chosenLAG)))$labels
  lts_hclustRow_order_feature2 <- hclust(dist(mCCF_chosenLAG))$order # get row order from clustered matrix and set this as a  variable
  lts_hclustColumn_LABELS_feature2 <-hclust(dist(mCCF_chosenLAG))$labels

  mCCF_chosenLAG[lts_hclustRow_order_feature2, lts_hclustColumn_order_feature1] #display matrix organised by rows and columns

  # get list of rows with names for ordering (order the factor this way)
  #original column ordering from matrix
  column_feature1 <- lts_hclustColumn_LABELS_feature1
  column_feature1

  #original row ordering from matrix
  row_feature2 <- lts_hclustColumn_LABELS_feature2
  row_feature2

  #reorder columns by clustering
  clust_column_feature1 <- column_feature1[lts_hclustColumn_order_feature1] #make a new or desired feature order based on the row order
  clust_column_feature1

  #reorder rows by clustering
  clust_row_feature2 <- row_feature2[lts_hclustRow_order_feature2] #make a new or desired feature order based on the row order
  clust_row_feature2

  #update main dataframe (update the factor levels for Treatments and Features, to be based on clustering)
  # reload ->join_ccflist_wMetaD #DELETE Th

  # organise cluster column levels
  join_ccflist_wMetaD_mode[.lts_variables$lts_compare_by[[1]] ] <- as.factor(join_ccflist_wMetaD_mode[,.lts_variables$lts_compare_by[[1]] ]) #make feature1 a factor
  join_ccflist_wMetaD_mode[.lts_variables$lts_compare_by[[1]] ] <- factor(join_ccflist_wMetaD_mode[,.lts_variables$lts_compare_by[[1]] ], levels =clust_column_feature1) #
  levels(join_ccflist_wMetaD_mode[,.lts_variables$lts_compare_by[[1]] ]) #check ordering

  # organise cluster row levels
  join_ccflist_wMetaD_mode[.lts_variables$lts_compare_by[[2]] ] <- as.factor(join_ccflist_wMetaD_mode[,.lts_variables$lts_compare_by[[2]] ]) #make feature1 a factor
  join_ccflist_wMetaD_mode[.lts_variables$lts_compare_by[[2]] ] <- factor(join_ccflist_wMetaD_mode[,.lts_variables$lts_compare_by[[2]] ], levels =clust_row_feature2) #
  levels(join_ccflist_wMetaD_mode[,.lts_variables$lts_compare_by[[2]] ]) #check ordering

  lts_clustered_ccflist <- join_ccflist_wMetaD_mode
  lts_mCCF_chosenLAG <- mCCF_chosenLAG

  lts_clusterOutput <- list(lts_clustered_ccflist = lts_clustered_ccflist,
                            lts_mCCF_chosenLAG = lts_mCCF_chosenLAG,
                            modeMaxCorrLAG = modeMaxCorrLAG) #this list output includes the matrix of values (mCCF) at chosen lag for dendrogram creation in plotting steps, an alternative is to generate this dendrogram denovo in a separate function or in the plotting step.

  # TODO: Try and use this as output
  # lts_clusterOutput <- list(list(lts_clustered_ccflist = lts_clustered_ccflist),
  #                           list(lts_mCCF_chosenLAG = lts_mCCF_chosenLAG),
  #                           list(modeMaxCorrLAG = modeMaxCorrLAG)) #this list output includes the matrix of values (mCCF) at chosen lag for dendrogram creation in plotting

  return(lts_clusterOutput)
}




#
#   #groups by categorical variaables and LAGs and gets mean correlation for each lag
#   meanCorrPerLag <- .lts_ccfWithMetaData %>%
#     dplyr::group_by(!!rlang::sym(.lts_compare_by[[1]]),
#                     !!rlang::sym(.lts_compare_by[[2]]),
#                     theLAG) %>% #group by vector of cluster groups
#     dplyr::summarise(meanCorrPerLag = mean(theCCF, na.rm = TRUE)) #summarise the mean at lag 0
#
#   join_ccflist_wMetaD <- dplyr::left_join(
#     .lts_ccfWithMetaData, meanCorrPerLag,
#     by = c(.lts_compare_by,
#            names(.lts_ccfWithMetaData[grepl("theLAG",names(.lts_ccfWithMetaData))]) #select column containing "theLAG"
#     )) # join mean lag to input dataframe for clustering
#
#   #remove features that are identical at zero lag #Todo, remove unused factor levels eg. coords
#   identicalMeasuresAt0 <- join_ccflist_wMetaD[join_ccflist_wMetaD$meanCorrPerLag ==1, ] #define identical measures
#   unq_identicalFeatures <- unique(identicalMeasuresAt0[,"theFeature"]) #added coma to subset the column and give a vector, not dataframe
#   if(nrow(identicalMeasuresAt0) > 0){print(paste("Warning: feature ",unq_identicalFeatures,"has correlation 1 at lag zero and will be removed"))
#     join_ccflist_wMetaD <- join_ccflist_wMetaD[!grepl(paste(unq_identicalFeatures, collapse="|"), join_ccflist_wMetaD$theFeature),]
#   } #remove features that are perfectly correlated
#
#   #get Lag with max correlation in the entire dataset
#   maxCorrLAG_total <-
#     unique(
#       join_ccflist_wMetaD[ #look in ccf with mean and metadata
#         join_ccflist_wMetaD$meanCorrPerLag == max(join_ccflist_wMetaD$meanCorrPerLag), #get row where max meanLAG
#         "theLAG"]) # in column "theLAG
#
#   #define mode function
#   Mode <- function(x) {
#     ux <- unique(x)
#     ux[which.max(tabulate(match(x, ux)))]
#   }
#
#   #get Lag with the mode max correlation
#   modeMaxCorrLAG <- join_ccflist_wMetaD %>%
#     dplyr::select(.lts_variables$lts_compare_by, theLAG, meanCorrPerLag)%>%
#     dplyr::group_by(dplyr::across(.lts_variables$lts_compare_by)) %>%
#     dplyr::top_n(1, meanCorrPerLag) %>%
#     unique() %>%
#     dplyr::pull(theLAG) %>%
#     Mode()
#
#
#   df_meanCorrAtModeMaxLAG <- join_ccflist_wMetaD %>%  #calculate mean Corr at Lag with mode max correlation
#     dplyr::filter(theLAG == modeMaxCorrLAG) %>%
#     dplyr::group_by(dplyr::across(.lts_variables$lts_compare_by)) %>% #group by vector of cluster groups
#     dplyr::summarise(meanCorrAtModeMaxLAG = mean(theCCF, na.rm = TRUE)) #summarise the mean at lag 0
#
#   join_ccflist_wMetaD_mode <- dplyr::left_join(
#     join_ccflist_wMetaD,
#     df_meanCorrAtModeMaxLAG,
#     by = c(.lts_compare_by) #no longer need to select column containing "theLAG"
#   )
