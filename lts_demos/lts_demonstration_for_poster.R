#testing script
#install devtools if needed
# if(!require("devtools")) install.packages("devtools")
#
#
# #########INSTALLATION#######
#
# #load devtools
# library(devtools)
#
# #install from github, using package access token
# install_github("somaSystems/lifeTimes",
# auth_token = "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")
#
# #run lifeTimes calculations for two categorical variables
# lts_default <- lts_in()
#
# lts_default$lts_ccfs_with_meta
#
#
#
# #######BATTERIES INCLUDED DEMONSTRATION ##############
#
# #plot results
# lts_plot_ccfs(lts_default)
#
# lts_plot_ClustSum(lts_default)
#
# lts_plot_coupled(lts_default)
#
# #run lifeTimes for one categorical variables
#
#
# lts_default$lts_ccf_summaries$lts_singleton_summ_metadata$

######BYOD: BRING YOUR OWN DATA ################

getwd()

#read cell data
lts_all_cells <- read.csv(file="lifeTimes/data-raw/clusterData.csv")

#get top 5 each cells
subsetCells <- lts_all_cells %>%
  dplyr::group_by(Treatment) %>%
  dplyr::select(cellNumber,Treatment) %>%
  unique() %>%
  slice_head(n = 5)

cellsToChoose <- subsetCells$cellNumber
cellsToChoose

sub_lts_all_cells <-lts_all_cells %>%
dplyr::filter( cellNumber %in% cellsToChoose)

write.csv(sub_lts_all_cells, "sampleCells.csv", row.names = FALSE)

####end data generation


###start example


# is_grouped_df(subsetCells)
# %>%
#
#
# subsetCells
#
# ?slice_head()
#
# subsetCells

lts_all_cells <- sub_lts_all_cells

head(lts_all_cells)


colnames(lts_extractedCells)

lts_cols_of_interest <-colnames(lts_extractedCells[,c(1:3)])

###start example
lts_cells <- read.csv(file = "data-raw/sampleCells.csv")

#look at colum names
colnames(lts_cells)

#choose three pairs of column names
pair1 <- c("Polarity_cell","Polarity_nucleus")
pair2 <- c("Eccentricity_cell", "Eccentricity_nucleus")
pair3 <- c("Volume_cell", "Volume_nucleus")

# use rbind to make these pairs into a matrix
my_pairs <- rbind(pair1,pair2,pair3)

#make these into list of pairs using helper function with "defined = TRUE"
lts_pairs <- lts_pairsMaker(my_pairs, defined = TRUE)


# str(my_pairs)
# typeof(my_pairs)
# dim(my_pairs)
# is.matrix(my_pairs)
lts_pairs <- lts_pairsMaker(my_pairs, defined = TRUE)

lts_pairs

lts_cols_of_interest

lts_pairs <- lts_pairsMaker(lts_cols_of_interest)


# enter arguments into lts_in
# here there is one categorical variable and multiple measured variables
lts_oneCat <- lts_in(.in_tsData = lts_cells,
                     .in_compare_categorical = "Treatment",
                     .in_time = "runNumber",
                     .in_plot_measured_variables = TRUE,
                     .in_pairedComparisons = lts_pairs,
                     .in_uniqueID_colname = "cellNumber",
                     .in_metaData = NULL )

#View results
lts_plot_ccfs(lts_oneCat)

lts_plot_ClustSum(lts_oneCat,  )
?lts_plot_coupled()
lts_plot_coupled(lts_oneCat,
                 .lts_colour_by = "cat2",
                 .lts_facet_by = "cat1")

# lts_oneCat <- lts_input(.tsData = lts_all_cells,
#                         .compare_categorical = "Treatment",
#                         .time = "runNumber",
#                         .plot_measured_variables = TRUE,
#                         .pairedComparisons = lts_pairs,
#                         .uniqueID_colname = "cellNumber",
#                         .metaData = NULL )

lts_oneCat <- lts_in(.in_tsData = lts_all_cells,
                        .in_compare_categorical = "Treatment",
                        .in_time = "runNumber",
                        .in_plot_measured_variables = TRUE,
                        .in_pairedComparisons = lts_pairs,
                        .in_uniqueID_colname = "cellNumber",
                        .in_metaData = NULL )


lts_oneCat$lts_variables$lts_data

lts_oneCat$lts_ccf_summaries$lts_catGroups_summ_modeMaxCorrLAG

lts_plot_ccfs(lts_oneCat)

lts_plot_ClustSum(lts_oneCat)

# grou
#                  .lts_facet_by = "cat1",
#                  .lts_colour_by = "cat2")

