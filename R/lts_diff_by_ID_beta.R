#' difference_by_unique_id
#'
#' Apply differencing to time series grouped by unique ID.
#'
#' @param data A dataframe containing the time series data. Defaults to the built-in dataset if not provided.
#' @param id_var The name of the unique ID column. Defaults to "key_num".
#' @param time_var The name of the time variable. Defaults to "dayOfseason".
#' @param measurement_vars A vector of names of the measurement variables. Defaults to `c("flow_m3s", "rainfall_cm")`.
#' @param diff_order An integer specifying the order of differencing (e.g., 1 for first difference, 2 for second difference, etc.). Defaults to 1.
#' @return A dataframe with the same layout as the input but with differenced values.
#' @export
#'

difference_by_unique_id <- function(
    data = NULL,
    id_var = "key_num",
    time_var = "dayOfseason",
    measurement_vars = c("flow_m3s", "rainfall_cm"),
    diff_order = 1
) {
  # Load required libraries
  # library(dplyr)
  # library(tidyr)

  # Load default data if not provided
  if (is.null(data)) {
    if (exists("lts_catchmentsAndRivers", envir = asNamespace("lifeTimes"))) {
      data <- get("lts_catchmentsAndRivers", envir = asNamespace("lifeTimes"))
    } else {
      stop("No input data provided, and default dataset 'lts_catchmentsAndRivers' is not available.")
    }
  }

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
  if (!is.numeric(diff_order) || diff_order < 1 || diff_order != round(diff_order)) {
    stop("Invalid input: diff_order must be a positive integer.")
  }

  # Helper function to apply multiple differencing
  apply_diff <- function(x, order) {
    for (i in seq_len(order)) {
      x <- c(NA, diff(x))
    }
    return(x)
  }

  # Apply differencing
  differenced_data <- data %>%
    dplyr::arrange(!!rlang::sym(id_var), !!rlang::sym(time_var)) %>% # Ensure proper time order
    dplyr::group_by(!!rlang::sym(id_var)) %>%
    dplyr::mutate(
      across(
        all_of(measurement_vars),
        ~ apply_diff(., diff_order),
        .names = "{.col}_diff"
      )
    ) %>%
    dplyr::ungroup()

  # Reshape the dataframe to match the original layout
  final_data <- differenced_data %>%
    dplyr::select(-all_of(measurement_vars)) %>% # Remove original values
    dplyr::rename_with(~ sub("_diff$", "", .), ends_with("_diff")) # Restore original column names

  return(final_data)
}



