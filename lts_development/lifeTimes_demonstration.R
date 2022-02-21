#testing script
#install devtools if needed
if(!require("devtools")) install.packages("devtools")

#load devtools
library(devtools)

#install from github, using package access token
install_github("somaSystems/lifeTimes",
auth_token = "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")

#run lifeTimes calculations for two categorical variables
lts_default <- lts_in()

#plot results
lts_plot_ccfs(lts_default)

lts_plot_ClustSum(lts_default)

lts_plot_coupled(lts_default)

#run lifeTimes for one categorical variables

lts_all_cells <- read.csv(file="data-raw/clusterData.csv")


head(lts_all_cells)

lts_cols_of_interest <-colnames(lts_extractedCells[,c(1:3)])

lts_cols_of_interest

lts_pairs <- lts_pairsMaker(lts_cols_of_interest)

lts_pairs

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

lts_plot_coupled(lts_oneCat,
                 .lts_facet_by = "cat1",
                 .lts_colour_by = "cat2")

