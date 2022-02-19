#todo --> have default flag to run the whole package in default data
#... this will remove some of the if "NULL" statements
# advanced template for adding paramaters

# get_ccf_summary
# Calculate summary statistics from CCFs

#TWO important TODO
# change type of clustering (wrapper to hclust)
# change feature that value for clustering is from

#####MAKE input data for function development######
library(lifeTimes)
library(magrittr)
lts <- lifeTimes:::lts_input(.tsData = catchmentsAndRivers,
                             .time = c("dayOfseason"),
                             .compare_categorical = c("season","catchmentRegion"), #Categorical variables
                             .plot_measured_variables = FALSE,
                             .pairedComparisons = list(
                               pair_1 =list(x ="flow_m3s" , y ="rainfall_cm" )), #pairedVarCCF
                             .uniqueID_colname = "key_num",
                             .metaData = NULL)

lts_wide <- lifeTimes:::lts_tsToWide(lts)
lts_ccf <- lifeTimes:::lts_wide_ts_to_ccf(lts_wide, .lts_variables = lts)
lts_df <- lifeTimes:::lts_ccf_df(lts_ccf, .lts_variables = lts)
lts_metadf <- lifeTimes:::lts_metaData_ccf_join(lts_df, .lts_variables = lts)
lts_cluster_out <- lifeTimes:::lts_clusterCCFs(lts_metadf, .lts_variables = lts)

lts_cluster_out$modeMaxCorrLAG

lts_sum_ccf <- lifeTimes::lts_summarise_ccf(lts_metadf, .lts_variables = lts) #it works
lts_sum_ccf$lts_catGroups_modeMaxCorrLAG
lts_sum_ccf$lts_singleton_modeMaxCorrLAG$allDataModeLag



######################

# lts_cluster_sum_stats <-function(
#   .lts_ccfWithMetaData = lts_sum_ccf,
#   .lts_variables = lts,
#   .chosenLAGforClustering = 1){


    .lts_ccfWithMetaData = lts_sum_ccf
    .lts_variables = lts
    .chosenLAGforClustering = lts_sum_ccf$lts_singleton_summ_modeMaxCorrLAG$allDataModeLag

  if(.lts_variables$lts_plot_measured_variables == TRUE){ #include "theFeature" in lts_compare_by, for plotting
    .lts_variables$lts_compare_by <- c(.lts_variables$lts_compare_by, "theFeature")
  } #Added this to have compare by "theFeature",

  if(is.null(.lts_variables)){
    .lts_variables <- lts_defaultVariables
  }

  .lts_compare_by <- .lts_variables$lts_compare_by




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





#create matrix of values using compare_by and chosen lag
#This step reduces everything to one value per set of categorical variables (representing many unique observations),
# this is the value that will be used for clustering

  lts_sum_ccf$lts_catGroups_summ_modeMaxCorrLAG
  lts_sum_ccf$lts_catGroups_summ
  # values_from ... this is the key step

  m_wide_lts_catGroups_summ <- lts_sum_ccf$lts_catGroups_summ_modeMaxCorrLAG %>% #make wider put treatment names
    tidyr::pivot_wider(
      id_cols = c(.lts_variables$lts_compare_by), #two categorical variables give unique ID
      names_from = .lts_variables$lts_compare_by[1], #lts_cluster_feature1 hereafter
      values_from = "catGroups_mean_corr_atModeLAG")

  # m_wide_join_ccflist_wMetaD <- join_ccflist_wMetaD_mode %>% #make wider, put treatment as colnames, put values as lag0
  # dplyr::filter(theLAG == .chosenLAGforClustering)%>% # choose lag that is mode most correlated lag
  # dplyr::select(.lts_variables$lts_compare_by, meanCorrPerLag)%>%
  # unique()%>%
  # tidyr::pivot_wider(
  #   id_col = c("meanCorrPerLag",.lts_variables$lts_compare_by),
  #   names_from = .lts_variables$lts_compare_by[1], #lts_cluster_feature1 hereafter
  #   values_from = "meanCorrPerLag")

mCCF_chosenLAG <- as.matrix(m_wide_lts_catGroups_summ[-1]) #make a numerical only matrix of mean correlation at lag zero, by removing first column
rownames(mCCF_chosenLAG) <-  m_wide_lts_catGroups_summ[[1]] # add rownames to matrix (#lts_cluster_feature2)

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

# Change factor levels in original
# Organise cluster column levels
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

