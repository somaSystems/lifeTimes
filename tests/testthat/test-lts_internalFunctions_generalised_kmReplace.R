library(lifeTimes)

test_that("dimensions of wide dataframe are correct based on variables input", {

  # lts_default <- lifeTimes:::dev_lts_inputNoCalls()
  lts_userInput <- lts_input()
  lts_data <- lts_userInput$lts_variables$lts_data
  lts_num_unique_obs <- length(unique(lts_data[,lts_userInput$lts_variables$lts_uniqueID_colname]))
  lts_num_features_to_compare <- length(unlist(lts_userInput$lts_variables$lts_pariedComparisons))
  lts_num_unique_obs
  lts_num_features_to_compare

  expect_equal(lts_num_unique_obs*lts_num_features_to_compare, ncol(lifeTimes:::lts_tsToWide(lts_userInput$lts_variables))-1)


})

