# tests/testthat/test-lts_in.R
library(testthat)

test_that("lts_in produces correct output", {
  # Example input
  example_input <- list(
    .in_tsData = lts_catchmentsAndRivers,  # Replace with a valid dataset
    .in_time = c("dayOfseason"),
    .in_compare_categorical = c("season", "catchmentRegion"),
    .in_pairedComparisons = list(pair_1 = list(y = "flow_m3s", x = "rainfall_cm")),
    .in_uniqueID_colname = "key_num"
  )
  
  # Run the function
  result <- do.call(revised_lts_in, example_input)
  
  # Check structure
  expect_type(result, "list")
  expect_named(result, c("lts_ts", "lts_ccfs", "lts_summary", "lts_cluster"))
  
  # Check intermediate results if applicable
  intermediate_results <- do.call(revised_lts_in, c(example_input, list(return_intermediate = TRUE)))
  expect_true("wide_data" %in% names(intermediate_results))
  expect_true("ccf_data" %in% names(intermediate_results))
})
