#' lifeTimesChain
#'
#' @importFrom magrittr %>%
#'
#' @description A function that generates default input and calls each of the lifeTimes functions
#' output is ccf from original time series and calculated differences
#'
#' @param timeSeriesData a time series dataset with observations, timepoints,
#' measured features, and labels for features to be coupled.
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

lifeTimesChain<- function(timeSeriesData = NULL){
  print(paste("timeSeriesData is null?",is.null(timeSeriesData)))
#If no data input generate default data
  if(is.null(timeSeriesData)){
    print("Running defaultData() to load test dataset...")
    timeSeriesData <- defaultData() #run function to make list of default data
    print(head(timeSeriesData$clustR))
    #Create values for arguments to tsToWide
    clustR <<- timeSeriesData$clustR
    featureSet1 <<- timeSeriesData$feaureSet1
    featureSet2 <<- timeSeriesData$featureSet2
    timePoints <<- timeSeriesData$timePoints
    maxTime <<- timeSeriesData$maxTimes
    metaData <<- timeSeriesData$metaData
  }

  #Extract metadata
# timeSeriesData$clustR %>%
define_tsMetaData() ->>  CellID_metaData_output #this goes into metaData_ccf_join

#generate ccfs
timeSeriesData$clustR %>%
  tsToWide() %>% #TODO: change this to take only one argument, move other arguments to "timesChain()"
  wide_ts_to_ccf()%>%
  metaData_ccf_join() %>%
  clusterCCFs() %>%
  leadLagCorr_diffs() -> timesChainOutput

return(timesChainOutput)
}


