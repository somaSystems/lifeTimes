# Load required packages
library(testthat)
library(lifeTimes)

# Example input
example_input <- list(
  .in_tsData = lts_catchmentsAndRivers,  # Ensure this dataset is valid
  .in_time = c("dayOfseason"),
  .in_compare_categorical = c("season", "catchmentRegion"),
  .in_pairedComparisons = list(pair_1 = list(y = "flow_m3s", x = "rainfall_cm")),
  .in_uniqueID_colname = "key_num"
)

# Define the original function
original_lts_in <- function(...) {
  lts_inputVars <- lifeTimes:::lts_input(...)

  lts_tsToWide(lts_inputVars) %>%
    lifeTimes:::lts_wide_ts_to_ccf(.lts_variables = lts_inputVars) %>%
    lifeTimes:::lts_ccf_df(.lts_variables = lts_inputVars) %>%
    lifeTimes:::lts_metaData_ccf_join(.lts_variables = lts_inputVars) %>%
    lifeTimes:::lts_summarise_ccf(.lts_variables = lts_inputVars) %>%
    lifeTimes:::lts_cluster_ccf_summs(.lts_variables = lts_inputVars) -> lts_Output

  return(lts_Output)
}

# Define the revised function
revised_lts_in <- function(...) {
  lts_inputVars <- list(...)

  wide_data <- lifeTimes:::lts_tsToWide(lts_inputVars)
  if (is.null(wide_data) || nrow(wide_data) == 0) {
    stop("Wide data is NULL or empty")
  }

  ccf_data <- lifeTimes:::lts_wide_ts_to_ccf(wide_data, .lts_variables = lts_inputVars)
  ccf_df <- lifeTimes:::lts_ccf_df(ccf_data, .lts_variables = lts_inputVars)
  ccf_with_meta <- lifeTimes:::lts_metaData_ccf_join(ccf_df, .lts_variables = lts_inputVars)
  ccf_summary <- lifeTimes:::lts_summarise_ccf(ccf_with_meta, .lts_variables = lts_inputVars)
  lts_Output <- lifeTimes:::lts_cluster_ccf_summs(ccf_summary, .lts_variables = lts_inputVars)

  return(lts_Output)
}

# Test: Internal Functions
test_that("Internal functions work correctly", {
  wide_data <- tryCatch({
    lifeTimes:::lts_tsToWide(example_input$.in_tsData)
  }, error = function(e) {
    fail(paste("Error in lts_tsToWide:", e$message))
  })
  expect_type(wide_data, "data.frame")  # Expected type for wide data

  ccf_data <- tryCatch({
    lifeTimes:::lts_cluster_ccf_summs(wide_data, example_input)
  }, error = function(e) {
    fail(paste("Error in lts_cluster_ccf_summs:", e$message))
  })
  expect_type(ccf_data, "list")  # Expected type for clustered data
})

# Test: Pipeline Functions
test_that("Pipeline functions produce equivalent outputs", {
  original_result <- tryCatch({
    do.call(original_lts_in, example_input)
  }, error = function(e) {
    fail(paste("Error in original_lts_in:", e$message))
  })

  revised_result <- tryCatch({
    do.call(revised_lts_in, example_input)
  }, error = function(e) {
    fail(paste("Error in revised_lts_in:", e$message))
  })

  expect_equal(original_result, revised_result)
})

