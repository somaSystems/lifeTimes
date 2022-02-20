#
# lts_cluster_ccf_summs <- function(
  .lts_ccf_with_summs = lts_summs
  .lts_variables = lts_inputVars

  # conditional for if only one categorical variable
  if(.lts_variables$lts_plot_measured_variables == TRUE){ #include "theFeature" in lts_compare_by, for plotting
    .lts_variables$lts_compare_by <- c(.lts_variables$lts_compare_by, "theFeature")
  } #Added this to have compare by "theFeature",

  #conditional to supply variables
  if(is.null(.lts_variables)){
    # .lts_variables <- lts_defaultVariables
    print("Input some .lts_variables into the 'lts_cluster_ccf_summs' function.")
  }

    # mapping arguments to function variables
  .lts_compare_by <- .lts_variables$lts_compare_by
  .lts_catGroups_sum_to_cluster <- .lts_ccf_with_summs$lts_ccf_summaries$lts_catGroups_summ_modeMaxCorrLAG
  .lts_ccf_with_meta <- .lts_ccf_with_summs$lts_ccfs_with_meta$lts_metadf


#create matrix of values using compare_by and chosen lag
#This step reduces everything to one value per set of categorical variables (representing many unique observations),
# this is the value that will be used for clustering


  # values_from ... this is the key step

  m_wide_lts_catGroups_summ <- .lts_catGroups_sum_to_cluster %>% #make wider put treatment names
    tidyr::pivot_wider(
      id_cols = c(.lts_variables$lts_compare_by[2]), #two categorical variables give unique ID
      names_from = .lts_variables$lts_compare_by[1], #lts_cluster_feature1 hereafter
      values_from = "catGroups_mean_corr_atModeLAG")

  # m_wide_lts_catGroups_summ

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

# lts_clust_ccfs_with_meta <- .lts_ccf_with_meta


#Build lists of output

# #list of new cluster output from this function
# lts_clustered <-list(
#   lts_clust_categoricals = list( clust_column_feature1 = clust_column_feature1, #the orders of categorical variables
#                                     clust_row_feature2 = clust_row_feature2),
#
#     lts_clust_ccfs_with_metadata = list(lts_clust_ccfs_with_meta = lts_clust_ccfs_with_meta) #the ccfs with factor levels ordered by clustering
# )
#
# lts_clustered_ccf_summs <- append(.lts_ccf_with_summaries, lts_clustered) #append new lists to lists from previous output
#
# lts_clustered_ccf_summs <- append(lts_clustered_ccf_summs,
#                                   list(lts_variables = .lts_variables))


lts_clustered_ccf_summs <- c(
  list( lts_variables = .lts_variables),
  .lts_ccf_with_summs, #this is already two named lsits so pasted in
  list(lts_clust_outputs = list( clust_matrix = mCCF_chosenLAG,
                                 clust_column_feature1 = clust_column_feature1, #the orders of categorical variables
                                 clust_row_feature2 = clust_row_feature2)
       ),
  list(lts_clust_ccfs_with_meta = .lts_ccf_with_meta) #the ccfs with factor levels ordered by clustering,
  )


return(lts_clustered_ccf_summs)
}


