library(lifeTimes)

# lts_march <- lts_in()
#
# lts_plot_ccfs(lts_march)
# lts_plot_ClustSum(lts_march)
# lts_plot_coupled(lts_march)

garmin <- read.csv(file = "cleaned_garmin.csv")
# View(garmin)
colnames(garmin)



# View(garmin)
library(dplyr)
sub_garmin <- garmin %>%
  dplyr::select(unq_key_garmin,
                two_min_time,
                session_split,
                latitude,
                longitude,
                altitude,
                distance,
                heart_rate,
                speed,
                cadence_cycling,
                power)

# View(sub_garmin)

# pairsVars <- colnames(sub_garmin[,c(6,8,9,11)])
colnames(sub_garmin)
pairsVars <- colnames(sub_garmin[,c(6,8:11)])


#
# colnames(sub_garmin)
#
# garmin$cut_trim_runDF
# colnames(sub_garmin)
# colnames(sub_garmin[,c(8:15)])
# cool_vars <- colnames(sub_garmin[,c(8:11)])

lts_pairs <- lts_pairsMaker(pairsVars)

str(lts_pairs)
garmin$unq_key

str(garmin)

# garmin$latitude

# lts_in()

lts_garmin <- lts_in(.in_tsData = sub_garmin,
                     .in_compare_categorical = "session_split",
                     .in_time = "two_min_time",
                     .in_plot_measured_variables = TRUE,
                     .in_pairedComparisons = lts_pairs,
                     .in_uniqueID_colname = "unq_key_garmin",
                     .in_metaData = NULL
)


lts_plot_ccfs(lts_garmin)
lts_plot_ClustSum(lts_garmin)
lts_plot_coupled(lts_garmin, .lts_facet_by = "cat1",.lts_colour_by = "cat2")

lts_pairs[(4:length(lts_pairs))]

lts <- lifeTimes:::lts_input(.tsData = sub_garmin,
                             .time = "two_min_time",
                             .compare_categorical = c("session_split"),
                             .plot_measured_variables = TRUE ,
                             .pairedComparisons = lts_pairs,
                             .uniqueID_colname = "unq_key_garmin",
                             .metaData = NULL)

wide <- lifeTimes:::lts_tsToWide(lts) ## can add an argument to remove NA


ccf <- lifeTimes:::lts_wide_ts_to_ccf(.lts_cast_ts = wide, .lts_variables = lts) # gives an error if there are na
ccf_df <- lifeTimes:::lts_ccf_df(.lts_ccflist = ccf,.lts_variables = lts)

meta_df <- lts_TEST_metaData_ccf_join(.lts_dfccf = ccf_df, .lts_variables = lts)

# meta_df <- lifeTimes:::lts_metaData_ccf_join(.lts_dfccf = ccf_df, .lts_variables = lts)
ccf_summs <- lifeTimes:::lts_summarise_ccf(.lts_ccfWithMetaData = meta_df,.lts_variables = lts)

# ccf_summs$lts_ccf_summaries

clust_summs <- lifeTimes:::lts_cluster_ccf_summs(.lts_ccf_with_summs =ccf_summs ,.lts_variables = lts)


lts_plot_ccfs(clust_summs)

# clust_summs$lts_ccf_summaries$lts_catGroups_summ

# colnames(wide)[colSums(is.na(wide)) > 0]
# colnames(wide)[g]
#
# colnames(wide[ , grepl( "key_40/" , names( wide ) ) ])
# colnames(wide[ , grepl( "key_2/" , names( wide ) ) ])
# # wide

#got an error in data because key was not unique

# View(wide)

# if(is.null(.lts_variables)){
# print("Enter variables in lts_tsToWide internal function")
# }
#
# .lts_variables = lts
#
# variablesToCompare <- unlist(.lts_variables$lts_pariedComparisons, use.names = FALSE)
#
# melt_ts <- .lts_variables$lts_data %>% tidyr::pivot_longer(
#   cols = variablesToCompare,
#   names_to = "melted_var",
#   values_to ="melted_measures"
# )
#
#
# # [1] "chosenObs_y: key_40 latitude"
# # [1] "chosenObs_x: key_40 longitude"
#
# melt_ts
#
# #This is already the final set of comparisons to make ()
# #cast using feature and observational unit (this includes)
# lts_cast_ts <- melt_ts %>% tidyr::pivot_wider(
#   id_cols = c(.lts_variables$lts_time), #timepoint uniquely identifies each observation (i.e each row)
#   names_from = c(.lts_variables$lts_uniqueID_colname,
#                  .lts_variables$lts_compare_by,
#                  melted_var),
#   names_sep = "/",
#   values_from = melted_measures
# ) # this gives a dataframe with nested lists of time series
# # cast_ts
#
#
# View(lts_cast_ts)
#
# wide <- lifeTimes:::lts_tsToWide(lts)

