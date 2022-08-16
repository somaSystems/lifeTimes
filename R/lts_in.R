#' lts_calc
#'
#' @param .in_tsData tidy time series data
#' @param .in_time name of the "time" variable
#' @param .in_compare_categorical names of categorical or explanatory variables to compare CCFs by. In future releases of this function, if there is only one explanatory variable, it will be possible to include a key of set of ".pairedComparisons".
#' @param .in_plot_measured_variables logical parameter, set to TRUE if using one categorical variables and want different CCFs plotted against a single categorical variable.
#' @param .in_pairedComparisons a single pair or list of pairs, of names of variables to generate cross correlations for.
#' @param .in_uniqueID_colname name of column with unique identifier
#' @param .in_lagMax maximum lag in CCFs
#' @param .in_clusterBy defaults to cluster by mean correlation at mode maximum correlated lag, otherwise clusters by "portion" that each grouping/facet of data represents as a total of category 1.
#' @param .in_metaData name of columns with metaData
#' @return calculated cross correlations, summary statistics and clustering that can be used for classification or plotting
#'
#' @export
#'

lts_in <- function(.in_tsData = lts_catchmentsAndRivers,
                       .in_time = c("dayOfseason"),
                       .in_compare_categorical = c("season","catchmentRegion"), #Categorical variables
                       .in_plot_measured_variables = FALSE,
                       .in_pairedComparisons = list(
                         pair_1 =list(y ="flow_m3s",x ="rainfall_cm")), #pairedVarCCF
                       .in_uniqueID_colname = "key_num",
                        .in_lagMax = NULL, #hotfix July 27 2022,
                       .in_clusterBy = NULL, #hotfix added August 2 2022
                        .in_differenced = FALSE, #hotfix added August 14 2022
                       .in_metaData = NULL
                        ){

  lts_inputVars <- lts_input(.tsData = .in_tsData,
                             .time = .in_time,
                             .compare_categorical = .in_compare_categorical, #Categorical variables
                             .plot_measured_variables =.in_plot_measured_variables,
                             .pairedComparisons = .in_pairedComparisons, #pairedVarCCF
                             .uniqueID_colname = .in_uniqueID_colname,
                             .lagMax = .in_lagMax,  #hotfix July 27 2022
                             .clusterBy = .in_clusterBy, #hotfix added August 2 2022
                             .differenced = .in_differenced, #hotfix added August 14 2022
                             .metaData = .in_metaData )

  lts_tsToWide(lts_inputVars) %>%
  lts_wide_ts_to_ccf(.lts_variables = lts_inputVars) %>%
  lts_ccf_df(.lts_variables = lts_inputVars) %>%
  lts_metaData_ccf_join(.lts_variables = lts_inputVars) %>%
  lts_summarise_ccf(.lts_variables = lts_inputVars) %>%
  lts_cluster_ccf_summs(.lts_variables = lts_inputVars) -> lts_Output
}
