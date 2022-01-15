#' dev_lts_FunctionCalls
#' @import magrittr
#' @importFrom magrittr "%>%"
#' @importFrom magrittr %>%
#' @param dev_lts_inputVars user description of variables taken from "lts_input()", otherwise taken from built in default set of variables.
#' @return dev version of output from all functions
#'

dev_lts_FunctionCalls <- function(dev_lts_inputVars){

dev_lts_cast_ts <<- dev_lts_tsToWide(.lts_variables = dev_lts_inputVars)

dev_lts_ccf_list <<- dev_lts_wide_ts_to_ccf(dev_lts_cast_ts, .lts_variables = dev_lts_inputVars)

dev_lts_dfccf <<- dev_lts_ccf_df(.dev_lts_ccf_list)

dev_lts_ccfWithMetaData_compareBy <<- dev_lts_metaData_ccf_join(dev_lts_dfccf, .lts_variables = dev_lts_inputVars)

dev_lts_clusterOutput <<- dev_lts_clusterCCFs(dev_lts_ccfWithMetaData_compareBy, .lts_variables = dev_lts_inputVars)

dev_lts_clusterOutput <<- dev_leadLagCorr_diffs(dev_lts_clusterOutput, .lts_variables = dev_lts_inputVars)

}
