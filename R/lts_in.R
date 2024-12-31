#' lts_in
#'
#' Description of the function's purpose.
#'
#' @param .in_tsData tidy time series data
#' @param .in_time name of the "time" variable
#' @param .in_compare_categorical names of categorical or explanatory variables to compare CCFs by. In future releases of this function, if there is only one explanatory variable, it will be possible to include a key of set of ".pairedComparisons".
#' @param .in_plot_measured_variables logical parameter, set to TRUE if using one categorical variables and want different CCFs plotted against a single categorical variable.
#' @param .in_pairedComparisons a single pair or list of pairs, of names of variables to generate cross correlations for.
#' @param .in_uniqueID_colname name of column with unique identifier
#' @param .in_lagMax maximum lag in CCFs
#' @param .in_clusterByPortions defaults to cluster by mean correlation at mode maximum correlated lag, otherwise clusters by "portion" that each grouping/facet of data represents as a total of category 1.
#' @param .in_metaData name of columns with metaData
#' @param return_intermediate Logical, return intermediate outputs. Default: FALSE.
#' @return calculated cross correlations, summary statistics and clustering that can be used for classification or plotting
#' @export

lts_in <- function(
    .in_tsData = NULL,
    .in_time = c("dayOfseason"),           # Time variable
    .in_compare_categorical = c("season", "catchmentRegion"), # Categorical variables
    .in_plot_measured_variables = FALSE,   # Option to plot measured variables
    .in_pairedComparisons = list(pair_1 = list(y = "flow_m3s", x = "rainfall_cm")), # Variable pairs for CCF
    .in_uniqueID_colname = "key_num",      # Column for unique IDs
    .in_lagMax = NULL,                     # Maximum lag for CCF
    .in_clusterByPortions = FALSE,         # Cluster by portions or mode max correlation
    .in_metaData = NULL,                   # Metadata columns
    return_intermediate = FALSE            # Option to return intermediate results
) {
  # Dynamically load default dataset if .in_tsData is not provided
  if (is.null(.in_tsData)) {
    data("lts_catchmentsAndRivers", package = "lifeTimes", envir = environment())
    .in_tsData <- lts_catchmentsAndRivers
  }

  # Ensure .in_tsData is a data frame
  .in_tsData <- as.data.frame(.in_tsData)

  # Convert specified columns to factors or characters as required
  if (!is.null(.in_compare_categorical)) {
    .in_tsData[.in_compare_categorical] <- lapply(
      .in_tsData[.in_compare_categorical],
      as.factor
    )
  }
  if (!is.null(.in_uniqueID_colname)) {
    .in_tsData[.in_uniqueID_colname] <- lapply(
      .in_tsData[.in_uniqueID_colname],
      as.character
    )
  }

  # Create input variable list
  lts_inputVars <- list(
    lts_data = .in_tsData,
    lts_time = .in_time,
    lts_compare_by = .in_compare_categorical,
    lts_plot_measured_variables = .in_plot_measured_variables,
    lts_pariedComparisons = .in_pairedComparisons,
    lts_uniqueID_colname = .in_uniqueID_colname,
    lts_lagMax = .in_lagMax,
    lts_clusterByPortions = .in_clusterByPortions,
    lts_metaData = .in_metaData
  )

  # Stepwise execution of the pipeline
  wide_data <- lts_tsToWide(lts_inputVars)
  ccf_data <- lts_wide_ts_to_ccf(wide_data, .lts_variables = lts_inputVars)
  ccf_df <- lts_ccf_df(ccf_data, .lts_variables = lts_inputVars)
  ccf_with_meta <- lts_metaData_ccf_join(ccf_df, .lts_variables = lts_inputVars)
  ccf_summary <- lts_summarise_ccf(ccf_with_meta, .lts_variables = lts_inputVars)
  lts_Output <- lts_cluster_ccf_summs(ccf_summary, .lts_variables = lts_inputVars)

  # Optionally return all intermediate results
  if (return_intermediate) {
    return(list(
      wide_data = wide_data,
      ccf_data = ccf_data,
      ccf_df = ccf_df,
      ccf_with_meta = ccf_with_meta,
      ccf_summary = ccf_summary,
      final_output = lts_Output
    ))
  }

  # Otherwise, return only the final output
  return(lts_Output)
}
