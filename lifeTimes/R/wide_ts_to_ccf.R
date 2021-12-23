# pairedCCFs!
#
# This is a function that creates cross correlation data for paired objects in a cell (or other primary object).
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

#2. Current Function takes a wide dataframe
#Column names require i)Unique cellID ii)feature being measured iii) compartment or object being measured
#to generalise change cell and nucleus to object1 and object2 (or leading and lagging object)
#to generalise, can just do compare every feature to every other feature for all features (But this is a lot?)

#3. After this create output from wide dataframe

#' Title
#'
#' @param wideIDfeatures
#'
#' @return
#' @export
#'
#' @examples
wide_ts_to_ccf <- function(wideIDfeatures){
  library(dplyr) #load library for filtering
  library(stringr)  #load library to split string

  #Part 1: This section takes the colnames and splits them into lists of cellIDs and list of feature measures
  listOfccfCellIDs <- unique(as.data.frame(str_split_fixed(colnames(wideIDfeatures[-1]), "_FeatureID_",2))[,1]) #get unique cellIDs from colnames of wide dataframe
  listOFccFFeatures <- unique(as.data.frame(str_split_fixed(colnames(wideIDfeatures[-1]), "_FeatureID_",2))[,2])#get cell Features from colnames of wide dataframe

  #Part 2: This section finds the feautres that are measured for BOTH cell and nucleus so that these can be compared in the function
  nc_listOFccFFeatures <-  as.data.frame(listOFccFFeatures) %>%  #make list of CCF features as a dataframe
  dplyr::filter(grepl("_cell|_nucleus",listOFccFFeatures))  #filter for only features that are cell and nucleus features

  sub_nc_listOFccFFeatures <- gsub("_nucleus|_cell","",nc_listOFccFFeatures$listOFccFFeatures) # remove nucleus and cell suffix from feature names
  vector_nc_listOFccFFeatures <- sub_nc_listOFccFFeatures   #reassign name for list of features list of features
  unq_vector_nc_listOFccFFeatures <- unique(vector_nc_listOFccFFeatures)   #make unique as features duplicated due to removal of Cell/nucleus suffix  29/11/21

  #Part 3: Start the Loop: This section creates an empty dataframe, and loops through each feature, and each cell, to generate correlations between cell and nucleus at lags
  df_setOfCCF_forEachFeatureAndCellID <- data.frame()  #create empty dataframe

  for(sub_chosenFeature in unq_vector_nc_listOFccFFeatures){   #loop through features in list

  an_chosenFeatureCell <- paste0(sub_chosenFeature,"_cell")  # make a cell version of each feature
  an_chosenFeatureNucleus <- paste0(sub_chosenFeature,"_nucleus") # make a nucleus version of each feature

  #Loop through all cells
  for(cellID in listOfccfCellIDs){
  print(paste("STARTing cell..",cellID, " With feature...",sub_chosenFeature)) #Print out position in the loop
  lookupCell <-   paste0(cellID,'_FeatureID_',an_chosenFeatureCell) # Create a unique label for cell and feature to get column by name
  lookupNucleus <-  paste0(cellID,'_FeatureID_',an_chosenFeatureNucleus) # Create a unique label for nucleus and feature  to get column by name

  #Select the two time series to be compared, by using the unique column names to select them
  #NB Currently using "pull" here to make data into vector isntead of dataframe but this function might be slow (unlist()) may be an alternative
  chosenTSone_y_cell <- pull(wideIDfeatures[,lookupCell]) # chosenTSone_y_cell
  choseTStwo_x_nucleus <- pull(wideIDfeatures[,lookupNucleus]) # chosenTSone_x_nucleus

  #Part 4: Run nucleus and cell components of a feature for the current cell, through the CCF function
  #Na is currently set to pass
  particularCellFeatureCCF <- ccf(chosenTSone_y_cell, choseTStwo_x_nucleus, plot = FALSE, na.action = na.pass)
  str(particularCellFeatureCCF)

  #Extract the output of the CCF function and create a dataframe entry
  anCCF_ACF <- particularCellFeatureCCF$acf #current CCF_correlation values
  anCCF_LAG <- particularCellFeatureCCF$lag #current CCF set of lags
  an_CCF_ObjectID <- paste(rep(cellID, length(particularCellFeatureCCF$lag))) # Object ID for current CCF
  an_CCF_Feature <- sub_chosenFeature # Feature name for current CCF
  particular_CCF_Object_output <- data.frame(anCCF_ACF , anCCF_LAG, an_CCF_ObjectID, an_CCF_Feature) # Bind these together as a "row" of a dataframe
  print(paste("adding Cell to dataframe...",cellID)) #print stage of loop

  df_setOfCCF_forEachFeatureAndCellID <- rbind(df_setOfCCF_forEachFeatureAndCellID, particular_CCF_Object_output) # add current output to main dataframe
  print(paste("ENDing cell..",cellID, " With feature...",sub_chosenFeature)) #print stage of loop

    } #exit cell ID loop and keep going through cell ID until have gone through all cells

  } # exit feature loop and keep going until have gone through all features

   return(df_setOfCCF_forEachFeatureAndCellID) # after comparing cell and nucleus for all feature return dataframe of CCFs
}
