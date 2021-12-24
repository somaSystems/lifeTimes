# Default dataset!

#run this function to use an inbuilt dataset for live analysis workflow

#Load default variables

#returning more than one value from a function in r
#https://stackoverflow.com/questions/1826519/how-to-assign-from-a-function-which-returns-more-than-one-value
#https://www.datamentor.io/r-programming/return-function/
defaultData <- function(){
# try(setwd("./data"))

#How to provide data
#https://r-pkgs.org/inst.html
#TODO: make external_clusterData.csv an .Rd file so it is smaller
 # clustR <- read.csv(system.file("extdata","external_clusterData.csv",package = "lifeTimes", mustWork = TRUE))
clustR <- read.csv(system.file("extdata","extractedCells.csv",package = "lifeTimes", mustWork = TRUE))

# clustR <<- read.csv(file = "./data/clusterData.csv")

feaureSet1 <- c("_cell") #features for one object type, marked by a suffix, these could also be contained in a metadata column called "object" #can also be a feature
featureSet2 <- c("_nucleus")#features for one object type, marked by a suffix, these could also be contained in a metadata column called "object" # can also be a feature
object1 <- c() # If there is an object ID column, name of object 1 type, eg. cell
object2 <- c() # If these is an object ID column, name of object 2 type eg. nucleus
features <- c() #list of features to compare between both objects
objectGroupings <<- c("cellNumber") #unique id for entity that groups two objects eg. cell ID number, treatment type #These can be got from metadata?
timePoints <- c("runNumber")#name of feature that is time measure
maxTimes <- c("46")
metaData <- c("cellNumber","fieldNumber","Treatment","Row","Column","Plate") # list of column names for metadata)

defaultData <- list(clustR = clustR,
                    feaureSet1 = feaureSet1,
                    featureSet2 = featureSet2,
                    object1 = object1,
                    object2 = object2,
                    features = features,
                    objectGroupings = objectGroupings,
                    timePoints = timePoints,
                    maxTimes = maxTimes,
                    metaData = metaData)

return(defaultData)
}




