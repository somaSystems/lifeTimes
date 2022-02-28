
garmin <- read.csv(file = "lts_garmin_data.csv")
# View(garmin)
colnames(garmin)

cool_vars <- colnames(garmin[,c(8:14)])

lts_pairs <- lts_pairsMaker(cool_vars)

str(lts_pairs)


str(garmin)

# garmin$latitude

# lts_in()

lts_garmin <- lts_in(.in_tsData = garmin,
        .in_compare_categorical = "session_fifths",
        .in_time = "time_num_zero_twoMinReset",
        .in_plot_measured_variables = TRUE,
        .in_pairedComparisons = lts_pairs,
       .in_uniqueID_colname = "cut_trim_runDF",
      .in_metaData = NULL
       )


lts_pairs[(4:length(lts_pairs))]

lts <- lifeTimes:::lts_input(.tsData = garmin,
                             .time = "time_num_zero_twoMinReset",
                             .compare_categorical = c("session_fifths"),
                             .plot_measured_variables = TRUE ,
                            .pairedComparisons = lts_pairs,
                            .uniqueID_colname = "cut_trim_runDF",
                            .metaData = NULL)


# garmin

wide <- lifeTimes:::lts_tsToWide(lts)


View(wide)

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

ccf <- lifeTimes:::lts_wide_ts_to_ccf(.lts_cast_ts = wide, .lts_variables = lts) # gives an error if there are na
# <- lifeTimes:::lts_ccf_df()
# <- lifeTimes:::lts_metaData_ccf_join()
