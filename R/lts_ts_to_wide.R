#' lts_tsToWide
#' Converts a tidy data frame into a wide format for time series analysis.
#' Each time series becomes a vector, uniquely identified by categorical and time variables.
#'
#' @importFrom magrittr "%>%"
#' @importFrom tidyr pivot_longer pivot_wider
#' @param .lts_variables A list of variables:
#' - lts_data: A tidy dataframe containing the time series data.
#' - lts_time: Name of the column representing the time variable.
#' - lts_uniqueID_colname: Column name containing unique identifiers.
#' - lts_compare_by: Vector of categorical variable names for grouping.
#' - lts_pariedComparisons: Variables to pivot and create wide-format data.
#' @return A dataframe in wide format with each time series as a vector.
#' @keywords internal

lts_tsToWide <- function(.lts_variables = NULL) {

  # Add error handling with clear messages (28/12/2024)
  if (is.null(.lts_variables)) {
    stop("Error: .lts_variables is NULL. Please provide the required input list.")
  }

  if (!"lts_data" %in% names(.lts_variables) || is.null(.lts_variables$lts_data)) {
    stop("Error: Missing or NULL 'lts_data' in .lts_variables. Ensure 'lts_data' is a valid dataframe.")
  }

  if (!"lts_time" %in% names(.lts_variables) || is.null(.lts_variables$lts_time)) {
    stop("Error: Missing or NULL 'lts_time' in .lts_variables. Specify the time variable name.")
  }

  if (!"lts_uniqueID_colname" %in% names(.lts_variables) || is.null(.lts_variables$lts_uniqueID_colname)) {
    stop("Error: Missing or NULL 'lts_uniqueID_colname' in .lts_variables. Provide a unique identifier column.")
  }

  if (!"lts_compare_by" %in% names(.lts_variables) || is.null(.lts_variables$lts_compare_by)) {
    stop("Error: Missing or NULL 'lts_compare_by' in .lts_variables. Provide categorical variables for grouping.")
  }

  if (!"lts_pariedComparisons" %in% names(.lts_variables) || is.null(.lts_variables$lts_pariedComparisons)) {
    stop("Error: Missing or NULL 'lts_pariedComparisons' in .lts_variables. Provide variables to compare.")
  }

  # Extract variables for clarity
  data <- .lts_variables$lts_data
  time_var <- .lts_variables$lts_time
  unique_id <- .lts_variables$lts_uniqueID_colname
  compare_vars <- .lts_variables$lts_compare_by
  variables_to_compare <- unlist(.lts_variables$lts_pariedComparisons, use.names = FALSE)

  # Ensure required columns exist in the data
  missing_cols <- setdiff(c(time_var, unique_id, compare_vars, variables_to_compare), names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Error: Missing columns in lts_data:", paste(missing_cols, collapse = ", ")))
  }

  # Convert data to long format
  # Added inline comments for clarity (28/12/2024)
  long_data <- data %>%
    tidyr::pivot_longer(
      cols = variables_to_compare,
      names_to = "melted_var",
      values_to = "melted_measures"
    )

  # Reshape into wide format using key identifiers
  wide_data <- long_data %>%
    tidyr::pivot_wider(
      id_cols = c(time_var),
      names_from = c(unique_id, compare_vars, "melted_var"),
      names_sep = "/",
      values_from = "melted_measures"
    )

  # Add metadata about transformation for debugging
  cat("Transformation complete: Long data dimensions:", dim(long_data), "| Wide data dimensions:", dim(wide_data), "\n")

  return(wide_data)
}
