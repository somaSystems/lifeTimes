#' lts_ts_to_wide
#' @importFrom magrittr "%>%"
#' @importFrom magrittr %>%
#' @import purrr
#' @import imputeTS
#' @param .lts_variables user description of variables taken from "lts_input()", otherwise taken from built in default set of variables.
#' @return a list that includes time series data, and strings from user input that map variables in the time series data to input in lifeTimes functions. Eg. which column of dataframe is the unit of "time", which is the categorical variables, and which are the variables to compare when generating CCFs.
#'

lts_tsToWide <- function(.lts_variables = NULL) {

  if(is.null(.lts_variables)){
    print("Enter variables in lts_tsToWide internal function")
  }

  variablesToCompare <- unlist(.lts_variables$lts_pariedComparisons, use.names = FALSE)

  melt_ts <- .lts_variables$lts_data %>% tidyr::pivot_longer(
    cols = variablesToCompare,
    names_to = "melted_var",
    values_to ="melted_measures"
  )

  #This is already the final set of comparisons to make ()
  #cast using feature and observational unit (this includes)
  lts_cast_ts <- melt_ts %>% tidyr::pivot_wider(
    id_cols = c(.lts_variables$lts_time), #timepoint uniquely identifies each observation (i.e each row)
    names_from = c(.lts_variables$lts_uniqueID_colname,
                   .lts_variables$lts_compare_by,
                   melted_var),
    names_sep = "/",
    values_from = melted_measures
  ) # this gives a dataframe with nested lists of time series
  # cast_ts

if(.lts_variables$imputeNA == TRUE){
  #Impute NA if selected
  wide_w_TNF_matrix_impute <- w_TNF_matrix %>%
    # filter()
    # select((where(is.numeric)))%>%
    # group_by(c_IDcellNumber_frame, geomFeature)%>%
    purrr::map_dfc(~na_ma(.x,k=1, maxgap =5))
}

  return(lts_cast_ts)
}
