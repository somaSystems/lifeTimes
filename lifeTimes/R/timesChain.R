# timeChain !

#battery of different timeSeries functions
# https://stackoverflow.com/questions/10291520/reading-all-scripts-and-data-files-from-multiple-folders

#a function that chains all the functions together
#currently using default input data

timesChain <- function(){

  library("rstudioapi")
setwd(dirname(getActiveDocumentContext()$path))  #setwd to same directory as timesChain (this may not work outside of Rstudio)

file.sources = list.files(pattern="*.R$", full.names=FALSE,
                          ignore.case=TRUE)

subset_file.sources<- file.sources[-(which(file.sources %in% "timesChain.R"))] #remove this script from list of sources

# sapply(subset_file.sources, source, .GlobalEnv)

for(script in subset_file.sources){
  setwd(dirname(getActiveDocumentContext()$path))
  print(paste("sourcing...",script))
  source(script)
}

#chain functions and output ccf
library(dplyr)
outputCCF <- defaultData() %>%
  clustR %>%
  tsToWide()%>%
  wide_ts_to_ccf()
}
#TODO: start using this output for downstream steps



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
#
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
