# tsMetaData!
#
# This is a function that takes input time series data and makes it wide.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#Overall schema
#1. Before this, make a function to create a wide dataframe


  tsToWide <- function(data = clustR, feature1 = feaureSet1, feature2 = featureSet2, timepoints = timePoints, maxTime = maxTimes) {
  library(rlang) # to convert variables to symbols of variables using sym(), and then evaluating these !!
  #https://stackoverflow.com/questions/48219732/pass-a-string-as-variable-name-in-dplyrfilter
  library(tidyr)

  long_clustR <- clustR %>% pivot_longer( #Make geomFeatures column of features containing object measurements
  cols = contains(feaureSet1) | #get measures for cols with object 1
  contains(featureSet2), #get measures for cols with object 2
  names_to = "geomFeatures")

  #make wide dataframe with columns that are cell ID and geom features, #keep only runNumber (timepoint, as an organising coolum) #keep only first "n" timepoints
  wide_cellID_geomFeature <- long_clustR %>%
  select(!!sym(timePoints),cellNumber,geomFeatures,value)%>%
  filter(!!sym(timePoints) < maxTimes) %>%
  pivot_wider(names_from = c("cellNumber","geomFeatures"),
                values_from = "value",
                names_sep = "_FeatureID_")

  return(wide_cellID_geomFeature)
  }


