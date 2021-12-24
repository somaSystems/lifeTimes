# outPutCCF !
# outputCCF <- timesChain()
# Processing CCF output (eg. link CCF back to treatments and metadata)


metaData_ccf_join <- function(outputCCF = df_setOfCCF_forEachFeatureAndCellID, CellID_metaData = CellID_metaData_output){
unq_metaData <- unique(CellID_metaData) #uses output of tsMetaData
outputCCF$an_CCF_ObjectID <- as.numeric(outputCCF$an_CCF_ObjectID) #make CCF object ID numeric
outputCCFdata_withMetaData <<- dplyr::left_join(outputCCF, unq_metaData, by = c("an_CCF_ObjectID"="cellNumber"))

return(outputCCFdata_withMetaData)
}

