#' lts_calc
#'
#' @param .in_tsData tidy time series data
#' @param .in_time name of the "time" variable
#' @param .in_compare_categorical names of categorical or explanatory variables to compare CCFs by. In future releases of this function, if there is only one explanatory variable, it will be possible to include a key of set of ".pairedComparisons".
#' @param .in_plot_measured_variables logical parameter, set to TRUE if using one categorical variables and want different CCFs plotted against a single categorical variable.
#' @param .in_pairedComparisons a single pair or list of pairs, of names of variables to generate cross correlations for.
#' @param .in_uniqueID_colname name of colum with unique identifier
#' @param .in_metaData name of columns with metaData
#'
#' @return calculated cross correlations, summary statistics and clustering that can be used for classification or plotting
#'
#' @export
#'

lts_calc <- function(.in_tsData = catchmentsAndRivers,
                       .in_time = c("dayOfseason"),
                       .in_compare_categorical = c("season","catchmentRegion"), #Categorical variables
                       .in_plot_measured_variables = FALSE,
                       .in_pairedComparisons = list(
                         pair_1 =list(x ="flow_m3s" , y ="rainfall_cm" )), #pairedVarCCF
                       .in_uniqueID_colname = "key_num",
                       .in_metaData = NULL){

  lts_inputVars <- lts_input(.tsData = .in_tsData,
                             .time = .in_time,
                             .compare_categorical = .in_compare_categorical, #Categorical variables
                             .plot_measured_variables =.in_plot_measured_variables,
                             .pairedComparisons = .in_pairedComparisons, #pairedVarCCF
                             .uniqueID_colname = .in_uniqueID_colname,
                             .metaData = .in_metaData )

  lts_tsToWide(lts_inputVars) %>%
  lts_wide_ts_to_ccf(.lts_variables = lts_inputVars) %>%
  lts_ccf_df(.lts_variables = lts_inputVars) %>%
  lts_metaData_ccf_join(.lts_variables = lts_inputVars) %>%
  lts_clusterCCFs(.lts_variables = lts_inputVars) %>%
  lts_leadLagCorr_diffs(.lts_variables = lts_inputVars) -> lts_Output
}
