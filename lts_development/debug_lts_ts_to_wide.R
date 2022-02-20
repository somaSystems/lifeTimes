

# lts_tsToWide <- function(

  .lts_variables = lts_inputVars


  if(is.null(.lts_variables)){
    print("Enter variables in lts_tsToWide internal function")
  }

  variablesToCompare <- unlist(.lts_variables$lts_pariedComparisons, use.names = FALSE)

  melt_ts <- .lts_variables$lts_data %>% tidyr::pivot_longer(
    cols = all_of(variablesToCompare), #select variables to compare#all_of() silences ambiguous external vector message
    names_to = "melted_var",
    values_to ="melted_measures"
  )

  colnames(melt_ts)

  #This is already the final set of comparisons to make ()
  #cast using feature and observational unit (this includes)
  lts_cast_ts <- melt_ts %>% tidyr::pivot_wider(
    id_cols = c(.lts_variables$lts_time), #timepoint uniquely IDs each observation
    names_from = c(.lts_variables$lts_uniqueID_colname,
                   .lts_variables$lts_compare_by,
                   melted_var),
    names_sep = "/",
    values_from = melted_measures
  ) # this gives a dataframe with nested lists of time series
  # cast_ts

  # return(lts_cast_ts)
# }


  ?pivot_wider()
