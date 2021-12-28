#Script to make Exported data in .RData format
#Stored in /data directory
#For use with package
defaultTimeSeries <- read.csv(file = "inst/extdata/external_clusterData.csv")
usethis::use_data(defaultTimeSeries)
