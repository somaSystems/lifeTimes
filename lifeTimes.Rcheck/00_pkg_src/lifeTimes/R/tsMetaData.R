# tsMetaData!

#select unique identifying columns and associated metadata
#This is where metadata are defined and chosen before CCF
# metaData <<- c("cellNumber","fieldNumber","Treatment","Row","Column","Plate") # list of column names for metadata

tsMetaData <- function(clustR, columsForMetaData = metaData) {
  library(rlang) # to convert variables to symbols of variables using sym(), and then evaluating these !!
  #https://stackoverflow.com/questions/48219732/pass-a-string-as-variable-name-in-dplyrfilter

  CellID_metaData <- clustR %>% #Make metadata chart
    select(all_of(metaData))

  # outputCCFdata_withMetaData <- metaData_ccf_join(outputCCF) #run metaData_ccf_join on output of CCF ans save results

  return(CellID_metaData)
}
