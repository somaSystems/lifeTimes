# timeChain !

#battery of different timeSeries functions
# https://stackoverflow.com/questions/10291520/reading-all-scripts-and-data-files-from-multiple-folders


try(setwd("./R"))

#a function that chains all the functions together
#currently using default input data

timesChain <- function(){

# library("rstudioapi")
# setwd(dirname(getActiveDocumentContext()$path))  #setwd to same directory as timesChain (this may not work outside of Rstudio)

file.sources = list.files(pattern="*.R$", full.names=FALSE,
                          ignore.case=TRUE)

subset_file.sources <- file.sources[-(which(file.sources %in% "timesChain.R"))] #remove this script from list of sources
# subset_file.sources <- subset_file.sources[-(which(subset_file.sources %in% "clusterCCFs.R"))]
subset_file.sources <- subset_file.sources[-(which(subset_file.sources %in% "inprogress_clusterCCFs.R"))]

subset_file.sources

# sapply(subset_file.sources, source, .GlobalEnv)

for(script in subset_file.sources){
  # setwd(dirname(getActiveDocumentContext()$path))
  print(paste("sourcing...", script))
  source(script)
}

#chain functions and output ccf
library(dplyr)

CellID_metaData <<-clustR %>% #assigns global variable metaData from original input
  define_tsMetaData()

defaultData() %>%
  clustR %>%
  tsToWide()%>%
  wide_ts_to_ccf()%>% #generates cross correlations from original input
  metaData_ccf_join() ->>
  outputCCFdata_withMetaData  #joined metadata to output

outputCCFdata_withMetaData %>% # NB: this stage can be a useful output
  clusterCCFs() ->>
  df_clusteredZeroLag_withMetada #NB: this stage can be a useful output

df_clusteredZeroLag_withMetada %>%
  leadLagCorr_diffs() ->> leadLagPlotInput

return(outputCCFdata_withMetaData) #returns cross correlations
}

outputCCF <- timesChain()

clusterPlot(plotType = "compoundPlot")
leadLagClusterPlot(leadLagPlotInput)
couplingPlot(leadLagPlotInput)


# clusterPlot()
# ?clusterPlot()
# # timesChain()



 plot("hello")
#Question, how best to handle that I would like to output metadata and ccf then join later
#Currently assigning metadata as global variable

#PREDO add install dependencies to script, and load libraries
#TODO: start using this output for downstream steps
# i. metaData_ccf_join
# ii.clusterCCFs
#iii. plotFromClustered

#
# source(file.sources[1])
#
# file.sources
# sapply(file.sources,source,.GlobalEnv)
#
# data.sources = list.files(c("C:/folder1", "C:/folder2"),
#                           pattern="*.rda$", full.names=TRUE,
#                           ignore.case=TRUE)
#
# wide_ts_to_ccf
#
# sapply(data.sources,load,.GlobalEnv)
# sapply(file.sources,source,.GlobalEnv)
#
# file.sources = list.files(path = "./R", pattern="*.R", full.names = FALSE)
# file.sources
# sapply(file.sources,source,.GlobalEnv)
#
# source(file = "R/defaultData.R")
#
# defaultData()
