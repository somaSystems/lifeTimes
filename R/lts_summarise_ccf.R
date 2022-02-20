#' lts_summarise_ccf
#'
#' @param .lts_ccfWithMetaData ccf with meta data
#' @param .lts_variables variables from user input into lts_calc() or lts_input()
#' @param .lts_portion selected portion of the maximum range of lags,
#' to be used for "portioned' summaries
#'
#' @return returns a list of ccf summaries
#' @export
#'
#'
#'


#TODO, for single units of observation with multiple features measured, calculate sum stats for each feature and pivot wider to get final measures

lts_summarise_ccf <-
  function(.lts_ccfWithMetaData = NULL,
                              .lts_variables = NULL,
                              .lts_portion = 0.25){ #for secondary set of summary stats

.lts_compare_by <- .lts_variables$lts_compare_by
.lts_uniqueID_colname <- .lts_variables$lts_uniqueID_colname

if(.lts_variables$lts_plot_measured_variables == TRUE){ #include "theFeature" in lts_compare_by, for plotting
  .lts_variables$lts_compare_by <- c(.lts_variables$lts_compare_by, "theFeature")
} #Added this to have compare by "theFeature",

#default variables if null
if(is.null(.lts_variables)){
  .lts_variables <- lts_defaultVariables
}

### calculate summary statistics for
## 1. the CCFs
## 2. the differenced CCFs (first derivative)

#start with CCF summaries
#get at observation level
#single observations summaries (one number per obs)
#so GROUP BY keynum

#Calculate DIFFERENCED CCFs
#calculate rates of change in correlation between lags (how quickly does correlation change)
#rate of change in  correlation as a function of
lts_metadf_diffed <- .lts_ccfWithMetaData %>%
  dplyr::group_by(key_num) %>%
  dplyr::mutate_at("theCCF", ~ . - dplyr::lag(., n = 1, default = NA),.keep = "all" )

#####TODO
# lts_metadf #CCFs
# lts_metadf_diffed #change in correlation per lag
###insert loop hear to generate summary stats on differen


### ONE: sum stats on unique observations,
### with one metric per unit of observation

lts_summ_key_num <- .lts_ccfWithMetaData %>%
  dplyr::group_by(key_num) %>%
  dplyr::summarise(mean_corr = mean(theCCF),
            max_corr = max(theCCF),
            min_corr = min(theCCF),
            var_corr = var(theCCF),
            sd_corr = sd(theCCF),
            median_corr = median(theCCF))

# calculate on unique obs for smaller range (portion) of the lags
lts_summ_key_num_portion <- .lts_ccfWithMetaData %>%
  dplyr::filter(
    (theLAG < (!!.lts_portion)*max(theLAG)) &
      (theLAG > (!!.lts_portion)*min(theLAG))
  ) %>%
  dplyr::group_by(key_num) %>%
  dplyr::summarise(mean_corr_lts_portion = mean(theCCF),
    max_corr_lts_portion = max(theCCF),
    min_corr_lts_portion = min(theCCF),
    var_corr_lts_portion = var(theCCF),
    sd_corr_lts_portion = sd(theCCF),
    median_corr_lts_portion = median(theCCF))

#update colnames to show the portion
#of max lag that the summary statistic is generated from appies to
colnames(lts_summ_key_num_portion) <-
  gsub(colnames(lts_summ_key_num_portion),
       pattern = "_lts_portion",
       replacement = paste0("_maxLag_",as.character(.lts_portion)))

### sum stats for single unique observations
# caclulated separately for negative, positive and zero lags

#calculate summary statistics separately for pre and post lag
#create labels for lag range
#create new variable of lag range,

#Define lags
lts_metadf_range <- .lts_ccfWithMetaData
lts_LAGmin <- min(lts_metadf_range$theLAG) #define LAGmin
lts_LAGmax <- max(lts_metadf_range$theLAG) #define LAGmax

#assign lags to observations
lts_metadf_range$lag_range <-
  ifelse(lts_metadf_range$theLAG >= lts_LAGmin &
   lts_metadf_range$theLAG < 0,"negativeLAG", # >= so max OR min not assigned as zero lag
   ifelse(lts_metadf_range$theLAG > 0 &
   lts_metadf_range$theLAG <= lts_LAGmax, "positiveLAG","zeroLAG"))

#group by unique observation, prior, post or zero, and get summary stats
lts_summ_lag_range <- lts_metadf_range %>%
  # dplyr::filter(lag_range != "zeroLAG") %>% #consider all lags (including zero)
  dplyr::group_by(key_num, lag_range) %>%
  dplyr::summarise(mean_corr_by_lag_range = mean(theCCF),
                   max_corr_by_lag_range = max(theCCF),
                   min_corr_by_lag_range = min(theCCF),
                   var_corr_by_lag_range = var(theCCF),
                   sd_corr_lag_by_range = sd(theCCF),
                   median_corr_by_lag_range = median(theCCF))

#a portion of group by unique observation, prior, post or zero, and get summary stats
lts_summ_lag_range_portion <- lts_metadf_range %>%
  dplyr::filter(
    (theLAG < (!!.lts_portion)*max(theLAG)) &
      (theLAG > (!!.lts_portion)*min(theLAG))
  ) %>%
  dplyr::group_by(key_num, lag_range) %>%
  dplyr::summarise(mean_corr_by_lag_range_lts_portion = mean(theCCF),
                   max_corr_by_lag_range_lts_portion = max(theCCF),
                   min_corr_by_lag_range_lts_portion = min(theCCF),
                   var_corr_by_lag_range_lts_portion = var(theCCF),
                   sd_corr_lag_by_range_lts_portion = sd(theCCF),
                   median_corr_by_lag_range_lts_portion = median(theCCF))

#update colnames to show the portion
#of max lag that the summary statistic is generated from appies to
colnames(lts_summ_lag_range_portion) <-
  gsub(colnames(lts_summ_lag_range_portion),
       pattern = "_lts_portion",
       replacement = paste0("_maxLag_",as.character(.lts_portion)))

# notes on across: https://stackoverflow.com/questions/64188671/renaming-multiple-columns-with-dplyr-renameacross
# calcualte diff between positive and negative lags, within each observation
#positive minus prior
lts_summ_lag_range_diffed <- lts_summ_lag_range %>%
  dplyr::filter(lag_range != "zeroLAG") %>% #only consider positive or negative lags
  dplyr::group_by(key_num)%>%
  dplyr::mutate_if(is.numeric, ~ . - dplyr::lag(., n = 1, default = NA),.keep = "all" ) %>%
  dplyr::select(where(is.numeric))%>%
  dplyr::rename_with(function(x) paste0("posNegDiff",x),where(is.numeric))%>%
  dplyr::group_by(key_num)%>%
  dplyr::slice(-1) #remove first observation (first lag of each)

# as above get diff, but start with the dataframe that is already from a portion of the data
#get within a certain portion of lag range
# calcualte diff between positive and negative lags, within each observation
#positive minus prior
lts_summ_lag_range_diffed_portion <- lts_summ_lag_range_portion %>% #the portioning is built in from a previous step
  dplyr::filter(lag_range != "zeroLAG") %>% #only consider positive or negative lags
  dplyr::group_by(key_num)%>%
  dplyr::mutate_if(is.numeric, ~ . - dplyr::lag(., n = 1, default = NA),.keep = "all" ) %>%
  dplyr::select(where(is.numeric))%>%
  dplyr::rename_with(function(x) paste0("posNegDiff",x),where(is.numeric)) %>%
  dplyr::group_by(key_num)%>%
  dplyr::slice(-1) #remove first observation (first lag of each)

##### JOIN single unit of observation summary statistics, by unique ID
# https://stackoverflow.com/questions/8091303/simultaneously-merge-multiple-data-frames-in-a-list
#joining summary statistics
#need to join all sum stats, some have two identifiers
#i.e unique_identifier and lag_range,
#others just have unique_identifier
#so first join the ones with multiple identifiers
#then join the ones that only have the single identifier
#make lists with reducing number of identifiers

#make a list of summary statistics for singleton observations UNGROUPED
lts_proto_summ_stats_list <-
list(lts_summ_key_num,
lts_summ_key_num_portion,
lts_summ_lag_range,
lts_summ_lag_range_portion,
lts_summ_lag_range_diffed,
lts_summ_lag_range_diffed_portion)

lts_indexOfDoubleID <- vector("list",length(lts_proto_summ_stats_list))
element <- 1
for(sums in lts_proto_summ_stats_list){
  lts_result_doubleID <- ("lag_range" %in% colnames(sums))
  print(lts_result_doubleID)
  lts_indexOfDoubleID[[element]] <- lts_result_doubleID
  element <- element+1
}

vector_indexOfDoubleID <-
  do.call(c,lts_indexOfDoubleID) #get the index positions with two unique descriptors

#join double list first
lts_singletonSummary_doubleKey <-
  Reduce(function(dtf1, dtf2) merge(dtf1, dtf2,
  by = c(.lts_uniqueID_colname, "lag_range"), all.x = TRUE),
  lts_proto_summ_stats_list[vector_indexOfDoubleID])

#add merged lists to a new list, and append the unmerged frames to this list
lts_sum_stats_list <-
  list(lts_singletonSummary_doubleKey)
lts_sum_stats_list <-append(lts_sum_stats_list,
    lts_proto_summ_stats_list[!vector_indexOfDoubleID]) #use inverted vector of columns

#join double list first
lts_singletonSummary <-
  Reduce(function(dtf1, dtf2) merge(dtf1, dtf2,
  by = c(.lts_uniqueID_colname), all.x = TRUE),
   lts_sum_stats_list)

###NOW JOIN categorical variables and Make group summary stats
unq_compareBy <-
  unique(.lts_variables$lts_data[, c(.lts_variables$lts_compare_by,
                                     .lts_variables$lts_uniqueID_colname)])

lts_singleton_summ_metadata <-
  merge(lts_singletonSummary,
  unq_compareBy,
  by = c(.lts_variables$lts_uniqueID_colname))  #categorical variables

###OUTPUT ONE
# lts_singletonSummary_metadata


### TWO SUMMARY STATISTICS BY CATEGORICAL GROUPS#####
#Here no more individuals (key num replaced by categoricals)
#for summaries , take summary of individual summaries.
#can have categorical per lag
#can  have categorical overall
#group by categories



#summarise by LAG, and by categorical variable
lts_catGroups_summ_key_num <- .lts_ccfWithMetaData %>%
  dplyr::group_by(!!rlang::sym(.lts_compare_by[[1]]),
                  !!rlang::sym(.lts_compare_by[[2]])) %>%
  dplyr::summarise(mean_corr = mean(theCCF),
                   max_corr = max(theCCF),
                   min_corr = min(theCCF),
                   var_corr = var(theCCF),
                   sd_corr = sd(theCCF),
                   median_corr = median(theCCF))


#calculate summary statiscs on CCF of individual unique observations
#over specified portion of the max lag (.lts_portion)
lts_catGroups_summ_key_num_portion <- .lts_ccfWithMetaData %>%
  dplyr::filter(
    (theLAG < (!!.lts_portion)*max(theLAG)) &
      (theLAG > (!!.lts_portion)*min(theLAG))
  ) %>%
  dplyr::group_by(!!rlang::sym(.lts_compare_by[[1]]),
                  !!rlang::sym(.lts_compare_by[[2]])) %>%
  dplyr::summarise(mean_corr_lts_portion = mean(theCCF),
                   max_corr_lts_portion = max(theCCF),
                   min_corr_lts_portion = min(theCCF),
                   var_corr_lts_portion = var(theCCF),
                   sd_corr_lts_portion = sd(theCCF),
                   median_corr_lts_portion = median(theCCF)) #%>% #add abs max lag to name in place of portion

colnames(lts_catGroups_summ_key_num_portion) <-
  gsub(colnames(lts_catGroups_summ_key_num_portion),
       pattern = "_lts_portion",
       replacement = paste0("_maxLag_",as.character(.lts_portion)))


#group by catGroup, prior, post or zero, and get summary stats
lts_catGroups_summ_lag_range <- lts_metadf_range %>%
  dplyr::group_by(!!rlang::sym(.lts_compare_by[[1]]),
                  !!rlang::sym(.lts_compare_by[[2]]),
                  lag_range) %>%
  dplyr::summarise(mean_corr_by_lag_range = mean(theCCF),
                   max_corr_by_lag_range = max(theCCF),
                   min_corr_by_lag_range = min(theCCF),
                   var_corr_by_lag_range = var(theCCF),
                   sd_corr_lag_by_range = sd(theCCF),
                   median_corr_by_lag_range = median(theCCF))
# lts_summ_lag_range
# lts_catGroups_summ_lag_range

#a portion of group by catGroup, prior, post or zero, and get summary stats
lts_catGroups_summ_lag_range_portion <- lts_metadf_range %>%
  # dplyr::filter(lag_range != "zeroLAG") %>% #consider all lags (including zero)
  dplyr::filter(
    (theLAG < (!!.lts_portion)*max(theLAG)) &
      (theLAG > (!!.lts_portion)*min(theLAG))
  ) %>%
  dplyr::group_by(!!rlang::sym(.lts_compare_by[[1]]),
                  !!rlang::sym(.lts_compare_by[[2]]),
                  lag_range) %>%
  dplyr::summarise(mean_corr_by_lag_range_lts_portion = mean(theCCF),
                   max_corr_by_lag_range_lts_portion = max(theCCF),
                   min_corr_by_lag_range_lts_portion = min(theCCF),
                   var_corr_by_lag_range_lts_portion = var(theCCF),
                   sd_corr_lag_by_range_lts_portion = sd(theCCF),
                   median_corr_by_lag_range_lts_portion = median(theCCF))

colnames(lts_catGroups_summ_lag_range_portion) <-
  gsub(colnames(lts_catGroups_summ_lag_range_portion),
       pattern = "_lts_portion",
       replacement = paste0("_maxLag_",as.character(.lts_portion)))

# notes on across: https://stackoverflow.com/questions/64188671/renaming-multiple-columns-with-dplyr-renameacross
# for catGroups calcualte diff between positive and negative lags, within each observation
#positive minus prior
lts_catGroups_summ_lag_range_diffed <- lts_catGroups_summ_lag_range %>%
  dplyr::filter(lag_range != "zeroLAG") %>% #only consider positive or negative lags
  dplyr::group_by(!!rlang::sym(.lts_compare_by[[1]]),
                  !!rlang::sym(.lts_compare_by[[2]])) %>%
  dplyr::mutate_if(is.numeric, ~ . - dplyr::lag(., n = 1, default = NA),.keep = "all" ) %>%
  dplyr::select(where(is.numeric))%>%
  dplyr::rename_with(function(x) paste0("posNegDiff",x),where(is.numeric))%>%
  dplyr::group_by(!!rlang::sym(.lts_compare_by[[1]]),
                  !!rlang::sym(.lts_compare_by[[2]])) %>%
  dplyr::slice(-1) #remove first observation (first lag of each)


# as above get diff, but start with the dataframe that is already from a portion of the data
#get within a certain portion of lag range
# calcualte diff between positive and negative lags, within each observation
#positive minus prior
lts_catGroups_summ_lag_range_diffed_portion <- lts_catGroups_summ_lag_range_portion %>% #the portioning is built in from a previous step
  dplyr::filter(lag_range != "zeroLAG") %>% #only consider positive or negative lags
  dplyr::group_by(!!rlang::sym(.lts_compare_by[[1]]),
                  !!rlang::sym(.lts_compare_by[[2]])) %>%
  dplyr::mutate_if(is.numeric, ~ . - dplyr::lag(., n = 1, default = NA),.keep = "all" ) %>%
  dplyr::select(where(is.numeric))%>%
  dplyr::rename_with(function(x) paste0("posNegDiff",x),where(is.numeric)) %>%
  dplyr::group_by(!!rlang::sym(.lts_compare_by[[1]]),
                  !!rlang::sym(.lts_compare_by[[2]])) %>%
  dplyr::slice(-1) #remove first observation (first lag of each)

# lts_catGroups_summ_lag_range_diffed_portion


#make a list of summary statistics for CATEGORICAL GROUPED SUMMARIES
lts_proto_catGroups_summ_stats_list <-
  list(lts_catGroups_summ_key_num,
       lts_catGroups_summ_key_num_portion,
       lts_catGroups_summ_lag_range,
       lts_catGroups_summ_lag_range_portion,
       lts_catGroups_summ_lag_range_diffed,
       lts_catGroups_summ_lag_range_diffed_portion)

lts_catGroups_indexOfDoubleID <- vector("list",length(lts_proto_catGroups_summ_stats_list))
element <- 1
for(sums in lts_proto_catGroups_summ_stats_list){
  lts_result_doubleID <- ("lag_range" %in% colnames(sums))
  print(lts_result_doubleID)
  lts_catGroups_indexOfDoubleID[[element]] <- lts_result_doubleID
  element <- element+1
}

vector_catGroups_indexOfDoubleID <-
  do.call(c,lts_catGroups_indexOfDoubleID)



#join double list first
lts_catGroups_Summary_doubleKey <-
  Reduce(function(dtf1, dtf2) merge(dtf1, dtf2,
                                    by = c(.lts_variables$lts_compare_by, "lag_range"), all.x = TRUE), #join by lag range and categorical variables
         lts_proto_catGroups_summ_stats_list[vector_catGroups_indexOfDoubleID])

#add merged lists to a new list, and append the unmerged frames to this list
lts_catGroups_sum_stats_list <-
  list(lts_catGroups_Summary_doubleKey)

lts_catGroups_sum_stats_list <-append(lts_catGroups_sum_stats_list,
                                     lts_proto_catGroups_summ_stats_list[!vector_catGroups_indexOfDoubleID]) #use inverted vector of columns

#join double list first
##SECOND OUTPUT
lts_catGroups_summ <-
  Reduce(function(dtf1, dtf2) merge(dtf1, dtf2,
                                    by = c(.lts_variables$lts_compare_by), all.x = TRUE),
         lts_catGroups_sum_stats_list)


###THREE get PER LAG summary stats for all of the data
####UNGROUPED AND GROUPED BY CATEGORY

lts_summ_perLAG <- .lts_ccfWithMetaData %>%
  dplyr::group_by(theLAG) %>%
  dplyr::summarise(mean_corr = mean(theCCF),
                   max_corr = max(theCCF),
                   min_corr = min(theCCF),
                   var_corr = var(theCCF),
                   sd_corr = sd(theCCF),
                   median_corr = median(theCCF))

#define mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


#Single OBS, by MAXCCF, LAG AT MAX, modeMAX LAG and CORR AT MODE MAX LAG

#get Lag with the mode (most frequent) max correlation for the entire dataset
lts_singleton_mut_modeMaxCorrLAG <- .lts_ccfWithMetaData %>%
  dplyr::select(theLAG, theCCF, !!rlang::sym(.lts_uniqueID_colname))%>% #group by unique ID
  dplyr::group_by(!!rlang::sym(.lts_uniqueID_colname))%>%
  dplyr::slice_max(theCCF, n =1, with_ties = FALSE) %>% #get the top CCF (max per group)
  dplyr::rename(maxCCF = theCCF, LAGatMaxCCF =theLAG)%>%
  dplyr::ungroup()%>% #ungroup to just get the Mode LAG of everything
  dplyr::mutate(allDataModeLag = Mode(LAGatMaxCCF), mean_corr_at_allDataModeLag = mean(maxCCF))

lts_singleton_summ_modeMaxCorrLAG <- .lts_ccfWithMetaData %>%
  dplyr::select(theLAG, theCCF, !!rlang::sym(.lts_uniqueID_colname))%>% #group by unique ID
  dplyr::group_by(!!rlang::sym(.lts_uniqueID_colname))%>%
  dplyr::slice_max(theCCF, n =1, with_ties = FALSE) %>% #get the top CCF (max per group)
  dplyr::rename(maxCCF = theCCF, LAGatMaxCCF =theLAG)%>%
  dplyr::ungroup()%>% #ungroup to just get the Mode LAG of everything
  dplyr::summarise(allDataModeLag = Mode(LAGatMaxCCF), mean_corr_at_allDataModeLag = mean(maxCCF))


# #get mean correlation across all of data set at lag with max mean correlation
# mean_corr_at_modeMaxCorrLAG <- lts_summ_perLAG %>%  #calculate mean Corr at Lag with mode max correlation
#   dplyr::filter(theLAG == -1) %>%
#   # dplyr::group_by(dplyr::across(.lts_variables$lts_compare_by)) %>% #group by vector of cluster groups
#   dplyr::summarise(meanCorrAtModeMaxLAG = mean(mean_corr, na.rm = TRUE)) #summarise the mean at lag 0
# mean_corr_at_modeMaxCorrLAG

### PER LAG BUT ALSO CATEGORICAL GROUPS####
#summarise by LAG, and by categorical variable
lts_catGroups_summ_perLAG <- .lts_ccfWithMetaData %>%
  dplyr::group_by(!!rlang::sym(.lts_compare_by[[1]]),
                  !!rlang::sym(.lts_compare_by[[2]]),
                  theLAG) %>%
  dplyr::summarise(mean_corr = mean(theCCF),
                   max_corr = max(theCCF),
                   min_corr = min(theCCF),
                   var_corr = var(theCCF),
                   sd_corr = sd(theCCF),
                   median_corr = median(theCCF))

#get Lag with the mode (most frequent) max correlation for the entire dataset
#starting with already summarised mean Corr per lag,
#group by categorical variables and take the top lag (there can only be top because of the grouping)

lts_catGroups_mut_modeMaxCorrLAG <- .lts_ccfWithMetaData %>%
  # dplyr::select(theLAG, theCCF, !!rlang::sym(.lts_uniqueID_colname))%>% #group by unique ID
  dplyr::group_by(!!rlang::sym(.lts_uniqueID_colname))%>% #group by unique IDS
  dplyr::slice_max(theCCF, n =1, with_ties = FALSE) %>% #get the top CCF for each ID
  dplyr::ungroup()%>% #ungroup to so just left with IDs and their max
  dplyr::rename(maxCCF = theCCF, LAGatMaxCCF =theLAG)%>% # rename the columns to reflect that they are Max values
  dplyr::group_by(!!rlang::sym(.lts_compare_by[[1]]), #group by categorical variables
                  !!rlang::sym(.lts_compare_by[[2]])) %>%
  # dplyr::ungroup()%>% #ungroup to just get the Mode LAG of everything
  dplyr::mutate(catGroups_ModeLAGatMax = Mode(LAGatMaxCCF), #get mode LAG with Max
                catGroups_mean_corr_atModeLAG = mean(maxCCF)) #get mean corr at mode Lag


lts_catGroups_summ_modeMaxCorrLAG <- .lts_ccfWithMetaData %>%
  # dplyr::select(theLAG, theCCF, !!rlang::sym(.lts_uniqueID_colname))%>% #group by unique ID
  dplyr::group_by(!!rlang::sym(.lts_uniqueID_colname))%>% #group by unique IDS
  dplyr::slice_max(theCCF, n =1, with_ties = FALSE) %>% #get the top CCF for each ID
  dplyr::ungroup()%>% #ungroup to so just left with IDs and their max
  dplyr::rename(maxCCF = theCCF, LAGatMaxCCF =theLAG)%>% # rename the columns to reflect that they are Max values
  dplyr::group_by(!!rlang::sym(.lts_compare_by[[1]]), #group by categorical variables
                  !!rlang::sym(.lts_compare_by[[2]])) %>%
  # dplyr::ungroup()%>% #ungroup to just get the Mode LAG of everything
  dplyr::summarise(catGroups_ModeLAGatMax = Mode(LAGatMaxCCF), #get mode LAG with Max
                catGroups_mean_corr_atModeLAG = mean(maxCCF)) #get mean corr at mode Lag


#recent added 12_2_2022
#Join mode max correlation information for single cells and group summaries
#back to the initial dataframes. This is for use in couplingPlot

# lts_singleton_summ_modeMax <-  merge(lts_singleton_summ_metadata, lts_singleton_summ_modeMaxCorrLAG, by = c(.lts_uniqueID_colname))
lts_catGroups_summ_modeMax <-  merge(lts_catGroups_summ, lts_catGroups_summ_modeMaxCorrLAG, by = c(.lts_compare_by))



# lts_catGroups_summ_modeMaxCorrLAG

lts_sum_ccf <- list(
  lts_ccfs_with_meta = list(lts_metadf = .lts_ccfWithMetaData),

  lts_ccf_summaries =list(

    lts_singleton_summ_metadata = lts_singleton_summ_metadata,
    # lts_singleton_summ_modeMax = lts_singleton_summ_modeMax, #recent added 12_2_2022
    lts_catGroups_summ = lts_catGroups_summ,
    lts_catGroups_summ_modeMax = lts_catGroups_summ_modeMax, #recent added 12_2_2022

    lts_summ_perLAG = lts_summ_perLAG,
    lts_singleton_mut_modeMaxCorrLAG = lts_singleton_mut_modeMaxCorrLAG,
    lts_singleton_summ_modeMaxCorrLAG = lts_singleton_summ_modeMaxCorrLAG,

    lts_catGroups_summ_perLAG = lts_catGroups_summ_perLAG,
    lts_catGroups_mut_modeMaxCorrLAG = lts_catGroups_mut_modeMaxCorrLAG,
    lts_catGroups_summ_modeMaxCorrLAG = lts_catGroups_summ_modeMaxCorrLAG
  )

                    )


return(lts_sum_ccf)
}
