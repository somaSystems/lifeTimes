# Load required packages
library(testthat)
library(lifeTimes)  # Ensure lifeTimes package is loaded

# Example input for the test
example_input <- list(
  .in_tsData = lts_catchmentsAndRivers,  # Replace with a valid dataset
  .in_time = c("dayOfseason"),
  .in_compare_categorical = c("season", "catchmentRegion"),
  .in_pairedComparisons = list(pair_1 = list(y = "flow_m3s", x = "rainfall_cm")),
  .in_uniqueID_colname = "key_num"
)

# Debugging: Access and test the internal functions directly
test_that("Internal functions work correctly", {
  # Test lts_tsToWide
  wide_data <- tryCatch({
    lifeTimes:::lts_tsToWide(example_input$.in_tsData)
  }, error = function(e) {
    fail(paste("Error in lts_tsToWide:", e$message))
  })
  expect_type(wide_data, "list")  # Replace "list" with the expected type

  # Test lts_cluster_ccf_summs
  ccf_data <- tryCatch({
    lifeTimes:::lts_cluster_ccf_summs(wide_data, example_input)
  }, error = function(e) {
    fail(paste("Error in lts_cluster_ccf_summs:", e$message))
  })
  expect_type(ccf_data, "list")  # Replace "list" with the expected type
})

# Debugging: Run the pipeline functions
test_that("Pipeline functions produce equivalent outputs", {
  original_result <- tryCatch({
    do.call(lifeTimes:::original_lts_in, example_input)
  }, error = function(e) {
    fail(paste("Error in original_lts_in:", e$message))
  })

  revised_result <- tryCatch({
    do.call(lifeTimes:::revised_lts_in, example_input)
  }, error = function(e) {
    fail(paste("Error in revised_lts_in:", e$message))
  })

  # Check equivalence of final outputs
  expect_equal(original_result, revised_result)
})
