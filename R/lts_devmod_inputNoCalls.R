#' dev_lts_inputNoCalls
#' @import magrittr
#' @importFrom magrittr "%>%"
#' @importFrom magrittr %>%
#' @param .tsData tidy time series data
#' @param .time name of the "time" variable
#' @param .compare_categorical names of categorical or explanatory variables to compare CCFs by. In future releases of this function, if there is only one explanatory variable, it will be possible to include a key of set of ".pairedComparisons".
#' @param .pairedComparisons a single pair or list of pairs, of names of variables to generate cross correlations for.
#' @param .uniqueID_colname name of colum with unique identifier
#' @param .metaData name of columns with metaData
#' @param .plot_measured_variables logical parameter, set to TRUE if using one categorical variables and want different CCFs plotted against a single categorical variable.
#'
#' @return a list that includes time series data, and strings from user input that map variables in the time series data to input in lifeTimes functions. Eg. which column of dataframe is the unit of "time", which is the categorical variables, and which are the variables to compare when generating CCFs.
#'

dev_lts_inputNoCalls <- function(.tsData = NULL,
                           .time = c("dayOfseason"),
                           .compare_categorical = c("season","catchmentRegion"), #Categorical variables
                           .plot_measured_variables = FALSE,
                           .pairedComparisons = list(
                             pair_1 =list(x = "rainfall_cm", y = "flow_m3s")), #pairedVarCCF
                           .uniqueID_colname = "key_num",
                           .metaData = NULL) {

  # if(is.null(.tsData)){.tsData <- load(file = "data/catchmentsAndRivers.rda")}
  if(is.null(.tsData)){.tsData <- catchmentsAndRivers}

    # read.csv(system.file("extdata","key_tidy_candr.csv",package = "lifeTimes", mustWork = TRUE)) #use this until internal data works

  lts_variables <- list(lts_data = .tsData, #create list of variables
                        lts_time = .time,
                        lts_compare_by = .compare_categorical,
                        lts_plot_measured_variables = .plot_measured_variables,
                        lts_pariedComparisons = .pairedComparisons,
                        lts_uniqueID_colname = .uniqueID_colname,
                        lts_metaData = .metaData)


  lts_variables$lts_data[,lts_variables$lts_compare_by ] <- lapply(   lts_variables$lts_data[,lts_variables$lts_compare_by ] , factor) #make compare_by variables, as factors
  dev_lts_inputVars  <<- lts_variables
return(dev_lts_inputVars)
}



  # return(dev_lts_Output)


