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
lts_cluster <- lifeTimes:::lts_cluster_ccf_summs(.lts_ccf_with_summs = lts_sum_ccf,.lts_variables = lts )

str(lts_cluster$lts_ccfs_with_meta)

str(lts_cluster$lts_clust_ccfs_with_metadata)


lts_sum_ccf$lts_catGroups_modeMaxCorrLAG
lts_sum_ccf$lts_singleton_modeMaxCorrLAG$allDataModeLag



######################

# lts_cluster_sum_stats <-function(
#   .lts_ccfWithMetaData = lts_sum_ccf,
#   .lts_variables = lts,
#   .chosenLAGforClustering = 1){
    .lts_ccf_with_summaries <- lts_sum_ccf

    .lts_variables = lts
    .chosenLAGforClustering = lts_sum_ccf$lts_singleton_summ_modeMaxCorrLAG$allDataModeLag

  if(.lts_variables$lts_plot_measured_variables == TRUE){ #include "theFeature" in lts_compare_by, for plotting
    .lts_variables$lts_compare_by <- c(.lts_variables$lts_compare_by, "theFeature")
  } #Added this to have compare by "theFeature",

  if(is.null(.lts_variables)){
    .lts_variables <- lts_defaultVariables
  }

  .lts_compare_by <- .lts_variables$lts_compare_by
  .lts_catGroups_sum_to_cluster <- lts_sum_ccf$lts_ccf_summaries$lts_catGroups_summ_modeMaxCorrLAG
  .lts_ccf_with_meta <- lts_sum_ccf$lts_ccfs_with_meta$lts_metadf


#create matrix of values using compare_by and chosen lag
#This step reduces everything to one value per set of categorical variables (representing many unique observations),
# this is the value that will be used for clustering


  # values_from ... this is the key step

  m_wide_lts_catGroups_summ <- .lts_catGroups_sum_to_cluster %>% #make wider put treatment names
    tidyr::pivot_wider(
      id_cols = c(.lts_variables$lts_compare_by), #two categorical variables give unique ID
      names_from = .lts_variables$lts_compare_by[1], #lts_cluster_feature1 hereafter
      values_from = "catGroups_mean_corr_atModeLAG")


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

# Change factor levels in original
# Organise cluster column levels

.lts_ccf_with_meta[.lts_variables$lts_compare_by[[1]] ] <- as.factor(.lts_ccf_with_meta[,.lts_variables$lts_compare_by[[1]] ]) #make feature1 a factor
.lts_ccf_with_meta[.lts_variables$lts_compare_by[[1]] ] <- factor(.lts_ccf_with_meta[,.lts_variables$lts_compare_by[[1]] ], levels =clust_column_feature1) #
levels(.lts_ccf_with_meta[,.lts_variables$lts_compare_by[[1]] ]) #check ordering

# organise cluster row levels
.lts_ccf_with_meta[.lts_variables$lts_compare_by[[2]] ] <- as.factor(.lts_ccf_with_meta[,.lts_variables$lts_compare_by[[2]] ]) #make feature1 a factor
.lts_ccf_with_meta[.lts_variables$lts_compare_by[[2]] ] <- factor(.lts_ccf_with_meta[,.lts_variables$lts_compare_by[[2]] ], levels =clust_row_feature2) #
levels(.lts_ccf_with_meta[,.lts_variables$lts_compare_by[[2]] ]) #check ordering

lts_clust_ccfs_with_meta <- .lts_ccf_with_meta

lts_clustered <-list(
  lts_clust_categoricals = list( clust_column_feature1 = clust_column_feature1,
                                    clust_row_feature2 = clust_row_feature2),

    lts_clust_ccfs_with_metadata = list(lts_clust_ccfs_with_meta = lts_clust_ccfs_with_meta)
)

lts_clustered_ccf_summs <- append(.lts_ccf_with_summaries, lts_clustered)
lts_clustered_ccf_summs$
return(lts_clustered_ccf_summs)
)


