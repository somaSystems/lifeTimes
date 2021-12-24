#' define_tsMetaData
#' @importFrom magrittr %>%
#'
#' @param clustR A time series dataset (eg.dataframe)
#' with variables labelling observations by;
#' observation ID number (currently = cellNumber),
#' fieldNumber, Treatment, Row (optional), Column (optional),
#' and Plate (optional).
#' @param columsForMetaData
#'
#' @return
#'
#'
#' @examples
#'

#select unique identifying columns and associated metadata
#This is where metadata are defined and chosen before CCF
# metaData <<- c("cellNumber","fieldNumber","Treatment","Row","Column","Plate") # list of column names for metadata
define_tsMetaData <- function(timeSeriesData = clustR , columsForMetaData = metaData) {
  # library(rlang) # to convert variables to symbols of variables using sym(), and then evaluating these !!
  # #https://stackoverflow.com/questions/48219732/pass-a-string-as-variable-name-in-dplyrfilter

  ensyms_metaData <- rlang::syms(columsForMetaData)
  # ensyms_metaData
  # sym(paste(ensyms_metaData, collapse= ","))

  # syms_columsForMetaData <- rlang::syms(columsForMetaData)



  CellID_metaData_output <- timeSeriesData %>% #Make metadata chart
    dplyr::select((paste(ensyms_metaData, sep= ",")))


  # outputCCFdata_withMetaData <- metaData_ccf_join(outputCCF) #run metaData_ccf_join on output of CCF ans save results

  return(CellID_metaData_output)
}

# (paste(metaData, collapse = "|"))
#
# metaData
# collapse_metaData <- (paste(metaData,collapse = "|"))
# sym(collapse_metaData)
# paste(syms_columsForMetaData, collapse = ",")

