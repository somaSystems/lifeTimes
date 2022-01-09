#' create deafault lts output
#' lts_defaultData
#'
#' This function loads a default catchmentsAndRivers.Rda,
#' and makes the "key_tidy_candr.csv" available to be read.
#' @return
#'
#'
#' @format example output of lts pipeline for input into plots
#'
#' @source lifeTimes package
#'
#' @examples lts_defaultOutputData()

lts_defaultOutputData <- function(){
usethis::use_data(lts_clusterOutput_LAGranges, overwrite = TRUE) #creates default lts output in data directory
}
