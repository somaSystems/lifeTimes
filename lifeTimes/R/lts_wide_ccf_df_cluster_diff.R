#' lts_wide_ts_to_ccf
#' @import magrittr
#' @importFrom magrittr "%>%"
#' @param .tsData tidy time series data
#' @param .time name of the "time" variable
#' @param .compare_categorical names of categorical or explanatory variables to compare CCFs by. In future releases of this function, if there is only one explanatory variable, it will be possible to include a key of set of ".pairedComparisons".
#' @param .pairedComparisons a single pair or list of pairs, of names of variables to generate cross correlations for.
#' @param .uniqueID_colname name of colum with unique identifier
#' @param .metaData name of columns with metaData
#'
#' @return a list that includes time series data, and strings from user input that map variables in the time series data to input in lifeTimes functions. Eg. which column of dataframe is the unit of "time", which is the categorical variables, and which are the variables to compare when generating CCFs.
#' @export
#'
#' @examples lts_defineVars(catchmentsAndRivers)
# examples lts_wide_ts_to_ccf(lts_cast_ts <- lts_tsToWide(),lts_variables <- lts_defineVars())
#'

# lifeTimesChain <- function(){

lts_defineVars <- function(.tsData = NULL,
                           .time = c("dayOfseason"),
                           .compare_categorical = c("season","catchmentRegion"), #Categorical variables
                           .pairedComparisons = list(
                             pair_1 =list(x = "rainfall_cm", y = "flow_m3s")), #pairedVarCCF
                           .uniqueID_colname = "key_num",
                           .metaData = NULL){

  # if(is.null(.tsData)){.tsData <- load(file = "data/catchmentsAndRivers.rda")}
  if(is.null(.tsData)){.tsData <- read.csv(system.file("extdata","key_tidy_candr.csv",package = "lifeTimes", mustWork = TRUE)) #use this until internal data works
  }


  # .tsData = catchmentsAndRivers
  # .time = c("dayOfseason")
  # .compare_categorical = c("season","catchmentRegion") #Categorical variables
  # .pairedComparisons = list(
  #   pair_1 =list(x = "rainfall_cm", y = "flow_m3s")) #pairedVarCCF
  # .uniqueID_colname = "key_num"
  # .metaData = NULL

  lts_variables <- list(lts_data = .tsData, #create list of variables
                        lts_time = .time,
                        lts_compare_by = .compare_categorical,
                        lts_pariedComparisons = .pairedComparisons,
                        lts_uniqueID_colname = .uniqueID_colname,
                        lts_metaData = .metaData)


  lts_variables$lts_data[,lts_variables$lts_compare_by ] <- lapply(   lts_variables$lts_data[,lts_variables$lts_compare_by ] , factor) #make compare_by variables, as factors



  return(lts_variables)
}
lts_variables <-lts_defineVars()



lts_tsToWide <- function(.lts_variables = lts_variables){

  # if(is.null(.lts_variables)){
  #   .lts_variables = lts_variables
  # }

  variablesToCompare <- unlist(.lts_variables$lts_pariedComparisons, use.names = FALSE)


  #tidyData_melt (melt compareVariables)
  melt_ts <- .lts_variables$lts_data %>% tidyr::pivot_longer(
    cols = variablesToCompare,
    names_to = "melted_var",
    values_to ="melted_measures"
  )
  # head(melt_ts)

  # nested_cast_ts <- melt_ts %>% tidyr::pivot_wider(
  #   id_cols = .lts_variables$lts_uniqueID_colname, #user input, analagous to cell ID, each thing is a unique time series measure
  #   names_from = melted_var,
  #   values_from = melted_measures
  # ) # this gives a dataframe with nested lists of time series
  # nested_cast_ts

  #This is already the final set of comparisons to make ()
  #cast using feature and observational unit (this includes)
  lts_cast_ts <- melt_ts %>% tidyr::pivot_wider(
    id_cols = c(.lts_variables$lts_uniqueID_colname,
                .lts_variables$lts_time) ,
    names_from = c(lts_variables$lts_uniqueID_colname,
                   .lts_variables$lts_compare_by,
                   melted_var),
    names_sep = "/",
    values_from = melted_measures
  ) # this gives a dataframe with nested lists of time series
  # cast_ts

  return(lts_cast_ts)
}
lts_cast_ts <<- lts_tsToWide()



lts_wide_ts_to_ccf<- function(.lts_cast_ts = lts_cast_ts, .lts_variables = lts_variables) {


if(missing(.lts_variables)){ lts_variables <- lts_defineVars()}
  # .cast_ts = cast_ts
  # .lts_variables = lts_variables


  .key_unqIDcombo <- unique(.lts_variables$lts_data[c("uniqueID","key_num")])
  .unqNameKey <- .key_unqIDcombo$uniqueID
  .unqNumKey <- .key_unqIDcombo$key_num

  .pairedComparisons =   lts_variables$lts_pariedComparisons

   #TODO fix this to make it dynamic
  lts_ccf_list <- vector("list", ncol(.lts_cast_ts[-1])/2)  # 1. output

  for(keyIndex in seq_along(.key_unqIDcombo$key_num)){ #get the index number of the key
    key_name <- .unqNameKey[keyIndex] #get the actual name descriptive name of the key instead of index number
    .key_num <- .unqNumKey[keyIndex] #get the numerical non descriptive key index

    for(pair in .pairedComparisons){ #key contains place and season (compare by), so now just do all paired comparisons
      print(paste("Started adding...", key_name, sep = "...")) #print stage of loop
      chosenObs_y <- .lts_cast_ts[,grepl(c(paste0(.key_num,"/")), names(.lts_cast_ts)) & # gets column with key_num
                                grepl(c(pair$y), names(.lts_cast_ts))]  # also gets column with pair y
      chosenObs_x <- .lts_cast_ts[,grepl(c(paste0(.key_num,"/")), names(.lts_cast_ts)) & # gets column with key_num
                                grepl(c(pair$x), names(.lts_cast_ts))] #sequence of 91 measures

      instanceOfCCF <- stats::ccf(chosenObs_y, chosenObs_x, plot = FALSE, na.action = na.pass) #calculate CCF for chosen pairing

      anCCF_ACF <- instanceOfCCF$acf #current CCF_correlation values
      anCCF_LAG <- instanceOfCCF$lag #current CCF set of lags
      an_CCF_ObjectID <- rep(.key_num, length(instanceOfCCF$lag)) # Object ID for current CCF
      an_CCF_Feature <-  rep(paste(pair$y,"versus",pair$x, sep="_"), length(instanceOfCCF$lag)) # Feature name for current CCF
      instanceOfCCF_Object_output <- list(theCCF = anCCF_ACF ,
                                          theLAG = anCCF_LAG, key_num = an_CCF_ObjectID,
                                          theFeature = an_CCF_Feature) # Bind these together as a "row" of a dataframe

      lts_ccf_list[[keyIndex]] <- instanceOfCCF_Object_output # add current output to main dataframe
      names(lts_ccf_list)[keyIndex] <- key_name

      print(paste("Finished adding...",key_name, sep = "...")) #print stage of loop
    }
  }
  return(lts_ccf_list)
}
lts_ccf_list_out <<- lts_wide_ts_to_ccf()



lts_ccf_df <- function(.lts_ccflist = lts_ccf_list_out){

  # if(missing(.lts_ccflist)){lts_ccflist <- lts_wide_ts_to_ccf((lts_cast_ts <- (lts_tsToWide())),(lts_variables <- lts_defineVars()) )}


  lts_protodf_list <- list(length(.lts_ccflist)) #define list

  for(key in seq_along(.lts_ccflist)){
    df_lts_obs <- as.data.frame((.lts_ccflist[[key]]))
    lts_protodf_list[[key]] <- df_lts_obs
  }

  lts_dfccf <- do.call(rbind, lts_protodf_list)

  return(lts_dfccf)
}
lts_dfccf <<- lts_ccf_df()





lts_metaData_ccf_join <- function(.lts_dfccf = lts_dfccf, .lts_variables = lts_variables){

  # .lts_dfccf = lts_dfccf
  # .lts_variables = lts_variables

  unq_compareBy <- unique(.lts_variables$lts_data[, c(.lts_variables$lts_compare_by, .lts_variables$lts_uniqueID_colname)]) #categorical variables

  lts_ccfWith_compareBy <- dplyr::left_join(.lts_dfccf, unq_compareBy, by = "key_num") #join categoricals

  if(!is.null(lts_variables$lts_metaData)){
    unq_metaData <- unique(.lts_variables$lts_data[, c(.lts_variables$lts_metaData, .lts_variables$lts_uniqueID_colname)])  #uses metaData
    lts_ccfWithMetaData_compareBy <<- dplyr::left_join(lts_ccfWith_compareBy, unq_metaData, by = "key_num")
  } else
  { lts_ccfWithMetaData_compareBy <- lts_ccfWith_compareBy
  }

  return(lts_ccfWithMetaData_compareBy)
}
lts_ccfWithMetaData_compareBy <<- lts_metaData_ccf_join()




lts_clusterCCFs <- function(.lts_ccfWithMetaData = lts_ccfWithMetaData_compareBy,
  .lts_variables = lts_variables,
  .chosenLAGforClustering = modeMaxCorrLAG){

  .lts_ccfWithMetaData = lts_ccfWithMetaData_compareBy
  # .lts_ccfWithMetaData = lts_ccfWithMetaData_compareBy
   # .lts_variables = lts_variables$lts_compare_by

   .lts_compare_by <- lts_variables$lts_compare_by


  #groups by categorical variaables and LAGs and gets mean correlation for each lag
  meanCorrPerLag <- .lts_ccfWithMetaData %>%
    dplyr::group_by(!!rlang::sym(.lts_compare_by[[1]]),
                    !!rlang::sym(.lts_compare_by[[2]]),
                      theLAG) %>% #group by vector of cluster groups
    dplyr::summarise(meanCorrPerLag = mean(theCCF, na.rm = TRUE)) #summarise the mean at lag 0

  join_ccflist_wMetaD <- dplyr::left_join(
    .lts_ccfWithMetaData, meanCorrPerLag,
    by = c(.lts_compare_by,
           names(.lts_ccfWithMetaData[grepl("theLAG",names(.lts_ccfWithMetaData))]) #select column containing "theLAG"
    )) # join mean lag to input dataframe for clustering

  #remove features that are identical at zero lag #Todo, remove unused factor levels eg. coords
  identicalMeasuresAt0 <- join_ccflist_wMetaD[join_ccflist_wMetaD$meanCorrPerLag ==1, ] #define identical measures
  if(nrow(identicalMeasuresAt0) > 0){print(paste("Warning: feature ",identicalMeasuresAt,"has correlation 1 at lag zero and will be removed"))
    join_ccflist_wMetaD <- join_ccflist_wMetaD[!grepl(paste(unq_identicalFeatures, collapse="|"), join_ccflist_wMetaD$theFeature),]
  } #remove features that are perfectly correlated

  #get Lag with max correlation in the entire dataset
  maxCorrLAG_total <-
    unique(
      join_ccflist_wMetaD[ #look in ccf with mean and metadata
        join_ccflist_wMetaD$meanCorrPerLag == max(join_ccflist_wMetaD$meanCorrPerLag), #get row where max meanLAG
        "theLAG"]) # in column "theLAG

  #define mode function
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }

  #get Lag with the mode max correlation
  modeMaxCorrLAG <- join_ccflist_wMetaD %>%
    dplyr::select(.lts_variables$lts_compare_by, theLAG, meanCorrPerLag)%>%
    dplyr::group_by(dplyr::across(.lts_variables$lts_compare_by)) %>%
    dplyr::top_n(1, meanCorrPerLag) %>%
    unique() %>%
    dplyr::pull(theLAG) %>%
    Mode()


  #### construction zone


  df_meanCorrAtModeMaxLAG <- join_ccflist_wMetaD %>%
    dplyr::filter(theLAG == modeMaxCorrLAG) %>%
    dplyr::group_by(dplyr::across(.lts_variables$lts_compare_by)) %>% #group by vector of cluster groups
    dplyr::summarise(meanCorrAtModeMaxLAG = mean(theCCF, na.rm = TRUE)) #summarise the mean at lag 0

  join_ccflist_wMetaD_mode <- dplyr::left_join(
    join_ccflist_wMetaD,
    df_meanCorrAtModeMaxLAG,
    by = c(.lts_compare_by) #no longer need to select column containing "theLAG"
  )


  ##end construction

  #calculate mean Corr at Lag with mode max correlation

  str(.lts_variables$lts_compare_by)
  str(join_ccflist_wMetaD_mode$meanCorrPerLag)


  str(join_ccflist_wMetaD_mode)
  #create matrix of values using compare_by and chosen lag
  #This step reduces everything to one lag value, which will allow for clustering
  m_wide_join_ccflist_wMetaD <- join_ccflist_wMetaD_mode %>% #make wider, put treatment as colnames, put values as lag0
    dplyr::filter(theLAG == .chosenLAGforClustering)%>% # choose lag that is mode most correlated lag
    dplyr::select(.lts_variables$lts_compare_by, meanCorrPerLag)%>%
    unique()%>%
    tidyr::pivot_wider(
      id_col = c("meanCorrPerLag",.lts_variables$lts_compare_by),
      names_from = .lts_variables$lts_compare_by[1], #lts_cluster_feature1 hereafter
      values_from = "meanCorrPerLag")

  mCCF_chosenLAG <- as.matrix(m_wide_join_ccflist_wMetaD[-1]) #make a numerical only matrix of mean correlation at lag zero, by removing first column
  rownames(mCCF_chosenLAG) <-  m_wide_join_ccflist_wMetaD[[1]] # add rownames to matrix (#lts_cluster_feature2)

  #cluster matrix
  lts_hclustColumn_order_feature1 <- hclust(dist(t(mCCF_chosenLAG)))$order # get column order from clustered matrix and set this as a variable
  lts_hclustColumn_LABELS_feature1 <-hclust(dist(t(mCCF_chosenLAG)))$labels
  lts_hclustRow_order_feature2 <- hclust(dist(mCCF_chosenLAG))$order # get row order from clustered matrix and set this as a  variable
  lts_hclustColumn_LABELS_feature2 <-hclust(dist(mCCF_chosenLAG))$labels

  mCCF_chosenLAG[lts_hclustRow_order_feature2, lts_hclustColumn_order_feature1] #display matrix organised by rows and columns

  # get list of rows with names for ordering (order the factor this way)
  # column_feature1 <- levels(lts_variables$lts_data[,.lts_variables$lts_compare_by[1]]) # make the features a variable called feature order, in the same order as for the matrix
  #original column ordering from matrix
  column_feature1 <- lts_hclustColumn_LABELS_feature1
  column_feature1

  #original row ordering from matrix
  row_feature2 <- lts_hclustColumn_LABELS_feature2
  row_feature2

  #reorder columns by clustering
  clust_column_feature1 <- column_feature1[lts_hclustColumn_order_feature1] #make a new or desired feature order based on the row order
  clust_column_feature1

  #reorder rows by clustering
  clust_row_feature2 <- row_feature2[lts_hclustRow_order_feature2] #make a new or desired feature order based on the row order
  clust_row_feature2

  #update main dataframe (update the factor levels for Treatments and Features, to be based on clustering)
  # reload ->join_ccflist_wMetaD #DELETE Th

  # organise cluster column levels
  join_ccflist_wMetaD_mode[.lts_variables$lts_compare_by[[1]] ] <- as.factor(join_ccflist_wMetaD_mode[,.lts_variables$lts_compare_by[[1]] ]) #make feature1 a factor
  join_ccflist_wMetaD_mode[.lts_variables$lts_compare_by[[1]] ] <- factor(join_ccflist_wMetaD_mode[,.lts_variables$lts_compare_by[[1]] ], levels =clust_column_feature1) #
  levels(join_ccflist_wMetaD_mode[,.lts_variables$lts_compare_by[[1]] ]) #check ordering

  # organise cluster row levels
  join_ccflist_wMetaD_mode[.lts_variables$lts_compare_by[[2]] ] <- as.factor(join_ccflist_wMetaD_mode[,.lts_variables$lts_compare_by[[2]] ]) #make feature1 a factor
  join_ccflist_wMetaD_mode[.lts_variables$lts_compare_by[[2]] ] <- factor(join_ccflist_wMetaD_mode[,.lts_variables$lts_compare_by[[2]] ], levels =clust_row_feature2) #
  levels(join_ccflist_wMetaD_mode[,.lts_variables$lts_compare_by[[2]] ]) #check ordering

  lts_clustered_ccflist <- join_ccflist_wMetaD_mode
  lts_mCCF_chosenLAG <- mCCF_chosenLAG

  lts_clusterOutput <- list(lts_clustered_ccflist = lts_clustered_ccflist,
                            lts_mCCF_chosenLAG = lts_mCCF_chosenLAG,
                            modeMaxCorrLAG = modeMaxCorrLAG) #this list output includes the matrix of values (mCCF) at chosen lag for dendrogram creation in plotting steps, an alternative is to generate this dendrogram denovo in a separate function or in the plotting step.

  return(lts_clusterOutput)
}
lts_clusterOutput <<- lts_clusterCCFs()




leadLagCorr_diffs <- function(
  .lts_clusterOutput = lts_clusterOutput,
  .lts_variables = lts_variables){
  # #
  # .lts_clusterCCFs = lts_clusterOutput
  # .lts_variables = lts_variables
  # .clusteredByChosenLAG <- .lts_clusterCCFs$lts_clustered_ccflist
  # .lts_clusterCCFs

  .clusteredByChosenLAG <-  .lts_clusterOutput$lts_clustered_ccflist

  lts_categoricalVariables <- c(.lts_variables$lts_compare_by[[1]],.lts_variables$lts_compare_by[[2]])
  # .lts_clusterCCFs
  # leadLagCorr_diffs

  # function to calculate median difference (asymmetry) in the mean of lead and lagging time points for different variables
  #TODO, make summaries of pre and post lags medians, rather than means

  #why is data being "arrnged" #NB thin I can delete this
  Arranged_lts_clusterCCFs <- .clusteredByChosenLAG %>% #arrange input dataframe
    dplyr::arrange(key_num, theFeature,
                   !!rlang::sym(lts_categoricalVariables[[1]]),
                   !!rlang::sym(lts_categoricalVariables[[2]]),
                   theLAG)%>% #arranging by LAG here is important
    dplyr::group_by(key_num, theFeature,
                    !!rlang::sym(lts_categoricalVariables[[1]]),
                    !!rlang::sym(lts_categoricalVariables[[2]]))
  # Arranged_lts_clusterCCFs

  #create new variable of lag range,

  LAGmin <- min(Arranged_lts_clusterCCFs$theLAG) #define LAGmin
  LAGmax <- max(Arranged_lts_clusterCCFs$theLAG) #define LAGmax

  Arranged_lts_clusterCCFs$lagRange <- # nested ifelse to set ranges of lags
    ifelse(Arranged_lts_clusterCCFs$theLAG >LAGmin & Arranged_lts_clusterCCFs$theLAG <0 , "negativeLAG",
           ifelse(Arranged_lts_clusterCCFs$theLAG >0 & Arranged_lts_clusterCCFs$theLAG <LAGmax, "positiveLAG","zeroLAG"))

  #get mean for lags before and after zero
  #first arrange by LAG, then calculate
  meanLagRange_lts_clusterCCFs <- Arranged_lts_clusterCCFs %>% # Organise by Cell ID, Feature, Treatment, and Lag (timepoint),
    dplyr::arrange(key_num,theFeature,
                   !!rlang::sym(lts_categoricalVariables[[1]]),
                   !!rlang::sym(lts_categoricalVariables[[2]]),
                   theLAG)%>%
    dplyr::group_by(key_num, theFeature,
                    !!rlang::sym(lts_categoricalVariables[[1]]),
                    !!rlang::sym(lts_categoricalVariables[[2]]),
                    lagRange) %>%
    dplyr::summarise(meanCorrforLAGrange = mean(theCCF, na.rm = TRUE)) # calculate average correlations in lags (either side of zero)

  #remove common names before joining, get vector of non common names from second dataframe and subset before joining
  varList<- names(meanLagRange_lts_clusterCCFs)[!(names(meanLagRange_lts_clusterCCFs) %in% names(Arranged_lts_clusterCCFs))] # get non common names
  varList

  #join mean CorrForLAGrange to Arranged_lts_clusterCCFs
  Arranged_lts_clusterCCFs <- dplyr::left_join(Arranged_lts_clusterCCFs,
                                               meanLagRange_lts_clusterCCFs[c(varList, "key_num")], by = "key_num")

  #make wider, taking names from lag range and values from mean lag
  #This gives a table of every, object (key_num), with average prior, post and zero lags calculated

  wider_meanLagRange_lts_clusterCCFs <- meanLagRange_lts_clusterCCFs %>%
    tidyr::pivot_wider(
      id_cols = c(key_num, theFeature, meanCorrforLAGrange,
                  !!rlang::sym(lts_categoricalVariables[[1]]),
                  !!rlang::sym(lts_categoricalVariables[[2]]),
      ),
      names_from = "lagRange",
      values_from = "meanCorrforLAGrange")
  wider_meanLagRange_lts_clusterCCFs

  wider_meanLagRange_lts_clusterCCFs$meanPrior_meanPost_diff <- #calculate difference in negative(prior) and positive(post) time lags
    wider_meanLagRange_lts_clusterCCFs$negativeLAG -
    wider_meanLagRange_lts_clusterCCFs$positiveLAG


  #remove common names before joining, get vector of non common names from second dataframe and subset before joining
  varList<- names(wider_meanLagRange_lts_clusterCCFs)[!(names(wider_meanLagRange_lts_clusterCCFs) %in% names(Arranged_lts_clusterCCFs))] # get non common names

  join_meanLagRange_Arranged_lts_clusterCCFs <- dplyr::left_join( #join prior to post ratio back to dataframe
    Arranged_lts_clusterCCFs,
    wider_meanLagRange_lts_clusterCCFs[c(varList,"key_num", #subset second dataframe to keep only: non common, and join by variables
                                         lts_categoricalVariables[[1]],
                                         lts_categoricalVariables[[2]])],
    by = c("key_num",
           lts_categoricalVariables[[1]],
           lts_categoricalVariables[[2]]))

  #check dimensions are the same
  dim(Arranged_lts_clusterCCFs) # wider_meanLagRange_join_outputCCFdata
  dim(join_meanLagRange_Arranged_lts_clusterCCFs)

  # remove common names before joining

  summaryOfMedianDifferencePriorAndPost <- #get overall feature mean for clustering
    join_meanLagRange_Arranged_lts_clusterCCFs %>%
    dplyr::group_by(!!rlang::sym(lts_categoricalVariables[[1]]),
                    !!rlang::sym(lts_categoricalVariables[[2]])) %>%
    dplyr::summarise(medianPrePostPerTF = median(meanPrior_meanPost_diff))


  join_medianDiff_meanLagRange_Arranged_lts_clusterCCFs <- #Join summary back to original dataframe, make a global variable
    dplyr::left_join(join_meanLagRange_Arranged_lts_clusterCCFs,
                     summaryOfMedianDifferencePriorAndPost[],
                     by = c(lts_categoricalVariables[[1]],
                            lts_categoricalVariables[[2]]))


  medDiff_meanLag_lts_clusterCCFs <- as.data.frame(join_medianDiff_meanLagRange_Arranged_lts_clusterCCFs)



  #consider appending to input list and returning appended list
  .lts_clusterOutput_withDiffs <- c(lts_clusterOutput,
                                    list(medDiff_meanLag_lts_clusterCCFs = medDiff_meanLag_lts_clusterCCFs))


  lts_clusterOutput_LAGranges <- .lts_clusterOutput_withDiffs

  return(lts_clusterOutput_LAGranges)
}

lts_clusterOutput_LAGranges <<- leadLagCorr_diffs()

return(lts_clusterOutput_LAGranges)


