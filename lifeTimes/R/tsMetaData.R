# tsMetaData!

#select unique identifying columns and associated metadata

metaData <- c("cellNumber","fieldNumber","Treatment","Row","Column","Plate") # list of column names for metadata

tsMetaData <- function(clustR, metaData) {
  library(rlang) # to convert variables to symbols of variables using sym(), and then evaluating these !!
  #https://stackoverflow.com/questions/48219732/pass-a-string-as-variable-name-in-dplyrfilter

  CellID_metaData <- clustR %>% #Make metadata chart
    select(metaData)

  return(wide_cellID_geomFeature)
}
