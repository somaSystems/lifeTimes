#' lts_lifeTimesChain
#'
#' @importFrom magrittr %>%
#' @description A function that generates default input and calls each of the lifeTimes functions
#' output is ccf from original time series and calculated differences
#'
#'
#' @return When run without entering an argument,
#' returns a named list including default timeseries data ($clustR),
#' and default arguments for tsToWide()
#'
#' @export
#'
#' @examples
#' outPutCCF <- lifeTimesChain()
#'
#'
#'

lifeTimesChain<- function(){

  lts_defineVars() %>%
    lts_tsToWide() %>%
    lts_wide_ts_to_ccf() %>%
    lts_ccf_df() %>%
    lts_metaData_ccf_join() %>%
    lts_clusterCCFs() %>%
    leadLagCorr_diffs() -> timesChainOutput

  return(timesChainOutput)

}


