#' lts_input
#'
#' @import magrittr
#' @importFrom magrittr "%>%"
#' @importFrom magrittr %>%
#' @param .tsData tidy time series data
#' @param .time name of the "time" variable
#' @param .compare_categorical names of categorical or explanatory variables to compare CCFs by. In future releases of this function, if there is only one explanatory variable, it will be possible to include a key of set of ".pairedComparisons".
#' @param .pairedComparisons a single pair or list of pairs, of names of variables to generate cross correlations for.
#' @param .plot_measured_variables logical parameter, set to TRUE if using one categorical variables and want different CCFs plotted against a single categorical variable.
#' @param .uniqueID_colname name of colum with unique identifier
#' @param .metaData name of columns with metaData
#'
#' @return a list that includes time series data, and strings from user input that map variables in the time series data to input in lifeTimes functions. Eg. which column of dataframe is the unit of "time", which is the categorical variables, and which are the variables to compare when generating CCFs.
#'




lts_input <-  function(.tsData = NULL,
                       .time = NULL,
                       .compare_categorical = NULL, #Categorical variables
                       .plot_measured_variables = FALSE,
                       .pairedComparisons =  NULL, #pairedVarCCF
                       .uniqueID_colname = NULL,
                       .metaData = NULL) {

# lts_input <-  function(.tsData = NULL,
#                        .time = c("dayOfseason"),
#                        .compare_categorical = c("season","catchmentRegion"), #Categorical variables
#                        .plot_measured_variables = FALSE,
#                        .pairedComparisons = list(
#                          pair_1 =list(x ="flow_m3s" , y ="rainfall_cm" )), #pairedVarCCF
#                        .uniqueID_colname = "key_num",
#                        .metaData = NULL) {

  if(is.null(.tsData)){.tsData <- catchmentsAndRivers}

  .tsData <- as.data.frame(.tsData) #could remove this?

  .tsData[.compare_categorical ] <- lapply(   .tsData[.compare_categorical ] , as.factor) #make compare_by variables, as factors

  lts_variables <- list(lts_data = .tsData, #create list of variables
                        lts_time = .time,
                        lts_compare_by = .compare_categorical,
                        lts_plot_measured_variables = .plot_measured_variables,
                        lts_pariedComparisons = .pairedComparisons,
                        lts_uniqueID_colname = .uniqueID_colname,
                        lts_metaData = .metaData)

  lts_inputVars <-lts_variables

  # lts_tsToWide(lts_inputVars) %>%
  #   lts_wide_ts_to_ccf(.lts_variables = lts_inputVars) %>%
  #   lts_ccf_df(.lts_variables = lts_inputVars) %>%
  #   lts_metaData_ccf_join(.lts_variables = lts_inputVars) %>%
  #   lts_clusterCCFs(.lts_variables = lts_inputVars) %>%
  #   lts_leadLagCorr_diffs(.lts_variables = lts_inputVars) -> lts_Output

  return(lts_inputVars)

}
