#' lts_join_metadata_to_ccf
#' @import magrittr
#' @importFrom magrittr "%>%"
#' @importFrom magrittr %>%
#' @param .lts_variables user description of variables taken from "lts_input()", otherwise taken from built in default set of variables.
#' @param .lts_dfccf output from lts_ccf_to_df
#' @return a list that includes time series data, and strings from user input that map variables in the time series data to input in lifeTimes functions. Eg. which column of dataframe is the unit of "time", which is the categorical variables, and which are the variables to compare when generating CCFs.
#'

lts_metaData_ccf_join <- function(.lts_dfccf = lts_dfccf, .lts_variables = NULL){


  if(is.null(.lts_variables)){
    .lts_variables <- lts_defaultVariables
  }

  unq_compareBy <- unique(.lts_variables$lts_data[, c(.lts_variables$lts_compare_by, .lts_variables$lts_uniqueID_colname)]) #categorical variables

  lts_ccfWith_compareBy <- dplyr::left_join(.lts_dfccf, unq_compareBy, by = c(.lts_variables$lts_uniqueID_colname )) #join categoricals





  if(!is.null(.lts_variables$lts_metaData)){
    unq_metaData <- unique(.lts_variables$lts_data[, c(.lts_variables$lts_metaData, .lts_variables$lts_uniqueID_colname)])  #uses metaData
    lts_ccfWithMetaData_compareBy <- dplyr::left_join(lts_ccfWith_compareBy, unq_metaData, by = .lts_variables$lts_uniqueID_colname)
  } else
  { lts_ccfWithMetaData_compareBy <- lts_ccfWith_compareBy
  }

  # if(sum(is.na(lts_ccfWithMetaData_compareBy$theCCF))>0){
  #   message("removing ccf for",)
  #
  # na_ccfrows <- lts_ccfWithMetaData_compareBy[is.na(lts_ccfWithMetaData_compareBy$theCCF),] #get rows where ccf is na
  # lts_ccfWithMetaData_compareBy[na_ccfrows,c( .lts_variables$lts_uniqueID_colname,"theFeature")]
  # filt_narm_meta_df <- lts_ccfWithMetaData_compareBy %>%
  #   filter(!is.na(theCCF))
  # }
  #

  return(lts_ccfWithMetaData_compareBy)
}
