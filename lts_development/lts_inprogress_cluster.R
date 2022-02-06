#clustering after summaries


#create matrix of values using compare_by and chosen lag
#This step reduces everything to one lag value, which will allow for clustering
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
