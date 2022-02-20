#' lts_internal_defaultData_defaultInput
#'
#' This function creats default lts_defaultVariables.Rda,
#'
#' @return
#'
#'
#' @format a dataframe with yearly rainfall_cm and riverflow, for the Ash river at Mardock,
#' and the Thames river at Kingston in the United Kingdom.
#'
#' @source
#'


# lts_defaultVariables<- function(){



# read external catchments and rivers data and make an internal version
# catchmentsAndRivers <- system.file("extdata", "key_tidy_candr.csv", package = "lifeTimes", mustWork = TRUE) # make key_tidy_candr.csv available to users
getwd()
catchmentsAndRivers <- read.csv(file="data-raw/key_tidy_candr.csv")
# key_tidy_candr.csv

#use lifeTimesInput to generate generate lifeTimesInput data
lts_makeInputVariables <- function(.tsData = catchmentsAndRivers,
                               .time = c("dayOfseason"),
                               .compare_categorical = c("season","catchmentRegion"), #Categorical variables
                               .pairedComparisons = list(
                                 pair_1 =list(x = "rainfall_cm", y = "flow_m3s")), #pairedVarCCF
                               .uniqueID_colname = "key_num",
                               .metaData = NULL){

  # # if(is.null(.tsData)){.tsData <- load(file = "data/catchmentsAndRivers.rda")}
  # if(is.null(.tsData)){.tsData <- read.csv(system.file("extdata","key_tidy_candr.csv",package = "lifeTimes", mustWork = TRUE)) #use this until internal data works
  # }

  lts_variables <- list(lts_data = .tsData, #create list of variables
                        lts_time = .time,
                        lts_compare_by = .compare_categorical,
                        lts_pariedComparisons = .pairedComparisons,
                        lts_uniqueID_colname = .uniqueID_colname,
                        lts_metaData = .metaData)


  lts_variables$lts_data[,lts_variables$lts_compare_by ] <- lapply(   lts_variables$lts_data[,lts_variables$lts_compare_by ] , factor) #make compare_by variables, as factors

  return(lts_variables)
  }


lts_defaultVariables <- lts_makeInputVariables()

lts_OUT_cast_ts <- lifeTimes:::lts_tsToWide(lts_defaultVariables)


lts_OUT_ccf_list_out <- lifeTimes:::lts_wide_ts_to_ccf(lts_OUT_cast_ts, .lts_variables = lts_defaultVariables)

lts_OUT_dfccf <- lifeTimes:::lts_ccf_df(lts_OUT_ccf_list_out)

lts_OUT_ccfWithMetaData_compareBy <- lifeTimes:::lts_metaData_ccf_join(lts_OUT_dfccf, .lts_variables = lts_defaultVariables)

lts_OUT_clusterOutput <- lifeTimes:::lts_clusterCCFs(lts_OUT_ccfWithMetaData_compareBy, .lts_variables = lts_defaultVariables)

lts_OUT_lts_clusterOutput_LAGranges <- lifeTimes:::lts_leadLagCorr_diffs(lts_OUT_clusterOutput, .lts_variables = lts_defaultVariables)


#store catchmentsAndRivers and lts_defaultVariables internally
usethis::use_data(catchmentsAndRivers,
                  lts_defaultVariables,
                  lts_OUT_cast_ts,
                  lts_OUT_ccf_list_out,
                  lts_OUT_dfccf,
                  lts_OUT_ccfWithMetaData_compareBy,
                  lts_OUT_clusterOutput,
                  lts_OUT_lts_clusterOutput_LAGranges,
                  internal = TRUE, overwrite = TRUE) #creates .rda data in data directory




# }
