#' adf_test_by_group
#'
#' Perform stationarity tests on grouped time series data using ADF test.
#'
#' @param data A dataframe containing the time series data.
#' @param time_var The name of the time variable.
#' @param id_var The name of the unique ID column.
#' @param measurement_vars A vector of names of the measurement variables.
#' @return A dataframe with ADF test results for each unique ID and measurement variable, with warnings if NA values were removed.
#' @export
#'

adf_test_by_group <- function(data, time_var, id_var, measurement_vars) {
  # Ensure required packages are available
  if (!requireNamespace("tseries", quietly = TRUE)) {
    stop("Package 'tseries' is required but is not installed. Please install it.")
  }
  # library(dplyr)
  # library(tidyr)

  # Ensure inputs are valid
  if (is.null(data) || !is.data.frame(data)) {
    stop("Invalid input: data must be a dataframe.")
  }
  if (is.null(time_var) || !(time_var %in% names(data))) {
    stop("Invalid input: time_var must be a valid column in the dataframe.")
  }
  if (is.null(id_var) || !(id_var %in% names(data))) {
    stop("Invalid input: id_var must be a valid column in the dataframe.")
  }
  if (is.null(measurement_vars) || !all(measurement_vars %in% names(data))) {
    stop("Invalid input: measurement_vars must be valid column names in the dataframe.")
  }

  # Create a tracker for NA removal logs
  na_removal_log <- data.frame(ID = character(), Variable = character(), NA_Removed = integer())

  # Reshape and calculate ADF test results
  results <- data %>%
    tidyr::pivot_longer(
      cols = all_of(measurement_vars),
      names_to = "Variable",
      values_to = "Value"
    ) %>%
    dplyr::group_by(!!rlang::sym(id_var), Variable) %>%
    dplyr::filter({
      na_count <- sum(is.na(Value))
      if (na_count > 0) {
        na_removal_log <<- rbind(na_removal_log,
                                 data.frame(ID = cur_group() %>% pull(!!rlang::sym(id_var)),
                                            Variable = cur_group() %>% pull(Variable),
                                            NA_Removed = na_count)
        )
      }
      !is.na(Value)
    }) %>%
    dplyr::summarise(
      ADF_Statistic = tryCatch(
        tseries::adf.test(Value, alternative = "stationary")$statistic,
        error = function(e) NA
      ),
      ADF_PValue = tryCatch(
        tseries::adf.test(Value, alternative = "stationary")$p.value,
        error = function(e) NA
      ),
      Stationarity = ifelse(!is.na(ADF_PValue) & ADF_PValue < 0.05, 1, 0),
      .groups = "drop"
    )

  # Issue a warning with the number of NA removals
  if (nrow(na_removal_log) > 0) {
    warning(paste("NA values removed from", nrow(na_removal_log), "groups. Use `attr(output, 'na_removals')` to inspect details."))
  }

  # Attach the NA removal log as an attribute for further inspection
  attr(results, "na_removals") <- na_removal_log

  return(results)
}

