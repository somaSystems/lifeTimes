
install.packages("lifeTimes")


####################### get data ######################################
lts_tnf <- read.csv(file = "data-raw/211021_TNF_washout_selected_tracks_FILTERED.csv")

colnames(lts_tnf)

library(ggplot2)


# hist(lts_tnf$Cell_Area, breaks = 100,)



ggplot(lts_tnf, aes(x = Time_min, y = lts_tnf$RELAratio, color = as.character(Position_TrackID), group = Position_TrackID))+
  geom_line()+
  xlim(0,200)

#subset so that max time is 500


#filter to remove tracks beyond 290

?subset()
f_lts_tnf <- subset(lts_tnf,lts_tnf$Time_min <300)

###subset Data
library(dplyr)
TNF_matrix <- f_lts_tnf %>% select(Time_min,RELAratio,Position_TrackID, contains("Cell"), contains("Nucleus"))


TNF_matrix$sizeSplit <- ifelse(TNF_matrix$Cell_Area > mean(TNF_matrix$Cell_Area),"LargerCell","SmallerCell")

library(tidyr)
dim(TNF_matrix)

colnames(w_TNF_matrix)


l_TNF_matrix <- TNF_matrix %>%
  pivot_longer(cols = contains("Cell") | contains("Nucleus") | contains("ratio"),
               names_to = "measure")

colnames(l_TNF_matrix)

w_TNF_matrix <- pivot_wider(l_TNF_matrix,
                            id_cols = Time_min,
                            names_from = c(Position_TrackID, measure),
                            values_from = value)

w_TNF_matrix
# ?pivot_wider()

# w_TNF_matrix


#Impute missing values
library(imputeTS)

library(dplyr)
library(imputeTS)

wide_w_TNF_matrix_impute <- w_TNF_matrix %>%
  # filter()
  # select((where(is.numeric)))%>%
  # group_by(c_IDcellNumber_frame, geomFeature)%>%
  purrr::map_dfc(~na_ma(.x,k=1, maxgap =5))

wide_w_TNF_matrix_impute


rl_TNF <- pivot_longer(wide_w_TNF_matrix_impute,
                       cols = c(2:length(wide_w_TNF_matrix_impute)),
                       names_to = "posID_measureName",
                       values_to = "measureValue")


TNF_measure_name <- strsplit(sub('(^[^_]+_[^_]+)_(.*)$', '\\1 \\2', rl_TNF$posID_measureName), ' ')
head(TNF_measure_name)

table_TNF_measure_name <- do.call(rbind,TNF_measure_name)


# df_TNF_measure_name <- data.frame(TNF_measure_name)


rl_TNF$posID <- table_TNF_measure_name[,1]
rl_TNF$measureName <- table_TNF_measure_name[,2]

# w_rl_TNF$Cell_Area
w_rl_TNF <- pivot_wider(rl_TNF,
                        id_cols = c(posID,Time_min),
                        names_from = measureName,
                        values_from = measureValue)


mw_rl_TNF <-w_rl_TNF %>%
  group_by(posID)%>%
  dplyr::mutate(avSize = mean(Cell_Area))


mw_rl_TNF$cellCategory <-  ifelse(mw_rl_TNF$avSize > median(mw_rl_TNF$avSize, na.rm = TRUE), "largerCell","smallerCell")
mw_rl_TNF$oneCategory <-  "aCell"

# w_rl_TNF$cellCategory

library(lifeTimes)

colnames(w_rl_TNF)

#pair RELAratio with every other

TNFvars <- mw_rl_TNF %>% dplyr::select(contains("Area"),contains("Solidity"),contains("Eccentricity"))%>%colnames()

TNFvars <- TNFvars[-1]

TNFratio <- rep("RELAratio",length(TNFvars))

myPairs <- cbind(TNFratio,TNFvars)

lts_RELApairs <- lts_pairsMaker(myPairs, defined = TRUE)

lts_RELApairs

mw_rl_TNF$posIDnew <- paste0(mw_rl_TNF$posID,mw_rl_TNF$cellCategory)

dim(mw_rl_TNF)
mw_rl_TNF <- mw_rl_TNF %>%
  filter(!is.na(cellCategory))
dim(mw_rl_TNF)
levels(as.factor(mw_rl_TNF$cellCategory))

#had error with labels changing categories

lts_RELA <- lts_in(.in_tsData = mw_rl_TNF,
                   .in_time = "Time_min",
                   .in_compare_categorical = "oneCategory",
                   .in_plot_measured_variables = TRUE,
                   .in_pairedComparisons = lts_RELApairs,
                   .in_uniqueID_colname = "posIDnew")

lts_RELA <- lts_in(.in_tsData = mw_rl_TNF,
                   .in_time = "Time_min",
                   .in_compare_categorical = "oneCategory",
                   .in_plot_measured_variables = TRUE,
                   .in_pairedComparisons = lts_RELApairs,
                   .in_uniqueID_colname = "posIDnew")


################################################################################


lts <- lifeTimes:::lts_input(.tsData = mw_rl_TNF,
                             .time = "Time_min",
                             .compare_categorical = c("oneCategory"),
                             .plot_measured_variables = TRUE ,
                             .pairedComparisons = lts_RELApairs,
                             .uniqueID_colname = "posIDnew",
                             .metaData = NULL)

wide <- lifeTimes:::lts_tsToWide(lts) ## can add an argument to remove NA

ccf <- lifeTimes:::lts_wide_ts_to_ccf(.lts_cast_ts = wide, .lts_variables = lts) # gives an error if there are na
ccf_df <- lifeTimes:::lts_ccf_df(.lts_ccflist = ccf,.lts_variables = lts)

# meta_df <- lts_TEST_metaData_ccf_join(.lts_dfccf = ccf_df, .lts_variables = lts)

meta_df <- lifeTimes:::lts_metaData_ccf_join(.lts_dfccf = ccf_df, .lts_variables = lts)
# ccf_summs <-lifeTimes:::lts_cluster_ccf_summs(.lts_ccfWithMetaData = meta_df,.lts_variables = lts)

ccf_summs <- lifeTimes:::lts_summarise_ccf(.lts_ccfWithMetaData = meta_df,.lts_variables = lts)

# ccf_summs$lts_ccf_summaries

clust_summs <- lifeTimes:::lts_cluster_ccf_summs(.lts_ccf_with_summs =ccf_summs ,.lts_variables = lts)


lts_plot_ccfs(clust_summs)

####################################################
# lts_cluster_ccf_summs <- function(
  .lts_ccf_with_summs = ccf_summs
  .lts_variables = lts

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
      id_cols = c(.lts_variables$lts_compare_by[2]), #use the categorical variable not chosen in names from
      names_from = .lts_variables$lts_compare_by[1], #lts_cluster_feature1 hereafter
      values_from = "catGroups_mean_corr_atModeLAG")


  m_wide_lts_catGroups_summ
  mCCF_chosenLAG

mCCF_chosenLAG <- as.matrix(m_wide_lts_catGroups_summ[-1]) #make a numerical only matrix of mean correlation at lag zero, by removing first column
rownames(mCCF_chosenLAG) <-  m_wide_lts_catGroups_summ[[1]] # add rownames to matrix (#lts_cluster_feature2)

#New section to cluster what can be clustered

colnames(mCCF_chosenLAG)
rownames(mCCF_chosenLAG)

#Feature 1
#if rows of transposed is > 1
  if(nrow(t(mCCF_chosenLAG)) >1){ # check if rows greater than 1 (this is first categorical variable)
    #use transposed matrix
    lts_hclustColumn_order_feature1 <- hclust(dist(t(mCCF_chosenLAG)))$order # get column order from clustered matrix and set this as a variable #BROKEN HERE
    lts_hclustColumn_LABELS_feature1 <-hclust(dist(t(mCCF_chosenLAG)))$labels} else{
    lts_hclustColumn_order_feature1 <- colnames(t(mCCF_chosenLAG)) # get column order from clustered matrix and set this as a variable #BROKEN HERE
    lts_hclustColumn_LABELS_feature1 <-colnames(t(mCCF_chosenLAG))
    }

#Feature 2
#if rows of normal is >1 (this is second categorical variable)
  # nrow(mCCF_chosenLAG)
if(nrow(t(mCCF_chosenLAG)) >1){ # check if rows greater than 1 (this is first categorical variable)
  #use untransposed matrix
  lts_hclustRow_order_feature2 <- hclust(dist(mCCF_chosenLAG))$order # get row order from clustered matrix and set this as a  variable
  lts_hclustColumn_LABELS_feature2 <-hclust(dist(mCCF_chosenLAG))$labels
} else{
  lts_hclustRow_order_feature2 <- colnames(mCCF_chosenLAG)  # get row order from clustered matrix and set this as a  variable
  lts_hclustColumn_LABELS_feature2 <-colnames(mCCF_chosenLAG)
}
# lts_hclustRow_order_feature2 <- hclust(dist(mCCF_chosenLAG))$order # get row order from clustered matrix and set this as a  variable
# lts_hclustColumn_LABELS_feature2 <-hclust(dist(mCCF_chosenLAG))$labels
lts_hclustColumn_order_feature1
lts_hclustColumn_LABELS_feature1
lts_hclustRow_order_feature2
lts_hclustColumn_LABELS_feature2

#   #cluster matrix
# lts_hclustColumn_order_feature1 <- hclust(dist(t(mCCF_chosenLAG)))$order # get column order from clustered matrix and set this as a variable #BROKEN HERE
# lts_hclustColumn_LABELS_feature1 <-hclust(dist(t(mCCF_chosenLAG)))$labels
# lts_hclustRow_order_feature2 <- hclust(dist(mCCF_chosenLAG))$order # get row order from clustered matrix and set this as a  variable
# lts_hclustColumn_LABELS_feature2 <-hclust(dist(mCCF_chosenLAG))$labels
# mCCF_chosenLAG[lts_hclustRow_order_feature2, lts_hclustColumn_order_feature1] #display matrix organised by rows and columns

# commented out hotfix march 25 2022
#sorry

# make a separate little bit for each of the clustering thing


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


# return(lts_clustered_ccf_summs)
# }


