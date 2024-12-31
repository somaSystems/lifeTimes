#' lts_input
#'
#' @importFrom magrittr "%>%"
#' @importFrom magrittr %>%
#' @param .tsData tidy time series data
#' @param .time name of the "time" variable
#' @param .compare_categorical names of categorical or explanatory variables to compare CCFs by. In future releases of this function, if there is only one explanatory variable, it will be possible to include a key of set of ".pairedComparisons".
#' @param .pairedComparisons a single pair or list of pairs, of names of variables to generate cross correlations for.
#' @param .plot_measured_variables logical parameter, set to TRUE if using one categorical variables and want different CCFs plotted against a single categorical variable.
#' @param .uniqueID_colname name of column with unique identifier
#' @param .lagMax maximum lag in CCFs
#' @param .clusterByPortions defaults to cluster by mean correlation at mode maximum correlated lag, otherwise clusters by "portion" that each grouping/facet of data represents as a total of category 1.
#' @param .metaData name of columns with metaData
#'
#' @return a list that includes time series data, and strings from user input that map variables in the time series data to input in lifeTimes functions. Eg. which column of dataframe is the unit of "time", which is the categorical variables, and which are the variables to compare when generating CCFs.
#' @export

lts_input <- function(
    .tsData = NULL,
    .time = NULL,
    .compare_categorical = NULL, # Categorical variables
    .plot_measured_variables = FALSE,
    .pairedComparisons = NULL, # pairedVarCCF
    .uniqueID_colname = NULL,
    .lagMax = NULL, # hotfix July 27 2022
    .clusterByPortions = FALSE, # hotfix August 2 2022
    .metaData = NULL
) {
  # Ensure .tsData is provided
  if (is.null(.tsData)) {
    stop("Error: .tsData is required but was not provided.")
  }

  # Convert to data frame if not already
  .tsData <- as.data.frame(.tsData)

  # Convert specified columns to factors or characters as required
  .tsData[.compare_categorical] <- lapply(
    .tsData[.compare_categorical],
    as.factor
  )
  .tsData[.uniqueID_colname] <- lapply(
    .tsData[.uniqueID_colname],
    as.character
  )

  # Create a list of variables
  lts_variables <- list(
    lts_data = .tsData,
    lts_time = .time,
    lts_compare_by = .compare_categorical,
    lts_plot_measured_variables = .plot_measured_variables,
    lts_pariedComparisons = .pairedComparisons,
    lts_uniqueID_colname = .uniqueID_colname,
    lts_lagMax = .lagMax, # hotfix July 27 2022
    lts_clusterByPortions = .clusterByPortions, # hotfix August 2 2022
    lts_metaData = .metaData
  )

  return(lts_variables)
}
