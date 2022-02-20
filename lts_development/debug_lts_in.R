

# lts_in <- function(
  .in_tsData = catchmentsAndRivers
                       .in_time = c("dayOfseason")
                       .in_compare_categorical = c("season","catchmentRegion") #Categorical variables
                       .in_plot_measured_variables = FALSE
                       .in_pairedComparisons = list(
                         pair_1 =list(y ="rainfall_cm",x ="flow_m3s")) #pairedVarCCF
                       .in_uniqueID_colname = "key_num"
                       .in_metaData = NULL

  lts_inputVars <- lts_input(.tsData = .in_tsData,
                             .time = .in_time,
                             .compare_categorical = .in_compare_categorical, #Categorical variables
                             .plot_measured_variables =.in_plot_measured_variables,
                             .pairedComparisons = .in_pairedComparisons, #pairedVarCCF
                             .uniqueID_colname = .in_uniqueID_colname,
                             .metaData = .in_metaData )

  lts_inputVars

wide <-   lifeTimes:::lts_tsToWide(lts_inputVars)
ccf <-  lifeTimes:::lts_wide_ts_to_ccf(wide, .lts_variables = lts_inputVars)
ccfdf <-   lifeTimes:::lts_ccf_df(ccf, .lts_variables = lts_inputVars)
 metadf <-  lifeTimes:::lts_metaData_ccf_join(ccfdf,.lts_variables = lts_inputVars)



 sum_ccf <- lifeTimes:::lts_summarise_ccf(metadf, .lts_variables = lts_inputVars)



clust_ccf <-  lifeTimes:::lts_cluster_ccf_summs(.lts_variables = lts_inputVars) -> lts_Output

