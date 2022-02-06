#todo --> have default flag to run the whole package in default data
#... this will remove some of the if "NULL" statements
# advanced template for adding paramaters

# get_ccf_summary
# Calculate summary statistics from CCFs

#####MAKE input data for function development######

lts <- lifeTimes:::lts_input(.tsData = catchmentsAndRivers,
                             .time = c("dayOfseason"),
                             .compare_categorical = c("season","catchmentRegion"), #Categorical variables
                             .plot_measured_variables = FALSE,
                             .pairedComparisons = list(
                               pair_1 =list(x ="flow_m3s" , y ="rainfall_cm" )), #pairedVarCCF
                             .uniqueID_colname = "key_num",
                             .metaData = NULL)

lts_wide <- lifeTimes:::lts_tsToWide(lts)
lts_ccf <- lifeTimes:::lts_wide_ts_to_ccf(lts_wide)
lts_df <- lifeTimes:::lts_ccf_df(lts_ccf)
lts_metadf <- lifeTimes:::lts_metaData_ccf_join(lts_df)

##### Plant functio args ####

lts_ccf_summary <- function (.lts_ccfWithMetaData = lts_metadf,
                             .lts_variables = lts)
######

# read in arguments
.lts_ccfWithMetaData = lts_metadf
.lts_variables = lts
.lts_portion <- 0.25 #for secondary set of summary stats
.lts_compare_by <- .lts_variables$lts_compare_by

if(.lts_variables$lts_plot_measured_variables == TRUE){ #include "theFeature" in lts_compare_by, for plotting
  .lts_variables$lts_compare_by <- c(.lts_variables$lts_compare_by, "theFeature")
} #Added this to have compare by "theFeature",

#default variables if null
if(is.null(.lts_variables)){
  .lts_variables <- lts_defaultVariables
}
#start with CCF summaries
#get at observation level

#single observations summaries (one number per obs)
#so GROUP BY keynum

library(magrittr)
lts_metadf



### calculate summary statistics for
## 1. the CCFs
## 2. the differenced CCFs (first derivative)

#Calculate DIFFERENCED CCFs
#calculate rates of change in correlation between lags (how quickly does correlation change)
#rate of change in  correlation as a function of
lts_differenced_summ_key_num_portion <- lts_metadf %>%
  dplyr::group_by(key_num) %>%
  dplyr::mutate_at("theCCF", ~ . - dplyr::lag(., n = 1, default = NA),.keep = "all" )
lts_differenced_summ_key_num_portion

###ONE_statistics on entire CCF per observation

# calculate summary statistics on CCF of individual unique observations
# and for a range around lag 0

lts_summ_key_num <- lts_metadf %>%
  dplyr::group_by(key_num) %>%
  dplyr::summarise(mean_corr = mean(theCCF),
            max_corr = max(theCCF),
            min_corr = min(theCCF),
            var_corr = var(theCCF),
            sd_corr = sd(theCCF),
            median_corr = median(theCCF))

# .lts_portion <- 0.25

# lts_filterTest <- lts_metadf %>%
#   dplyr::filter(
#     (theLAG < (!!.lts_portion)*max(theLAG)) &
#       (theLAG > (!!.lts_portion)*min(theLAG))
#   )
# lts_filterTest

#calculate summary statiscs on CCF of individual unique observations
#over specified portion of the max lag (.lts_portion)
lts_summ_key_num_portion <- lts_metadf %>%
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
                   median_corr_lts_portion = median(theCCF)) #%>% #add abs max lag to name in place of portion
                   # gsub(colnames(.), pattern = "_lts_portion", replacement = as.character(.lts_portion))

#update colname to show the portion of max lag that the summary statistic appies to
colnames(lts_summ_key_num_portion) <- gsub(colnames(lts_summ_key_num_portion), pattern = "_lts_portion", replacement = paste0("_maxLag_",as.character(.lts_portion)))



###TWO_ statistics caclulated for negative and positive lags per observation
## and for a range around lag 0


#calculate summary statistics separately for pre and post lag
#create labels for lag range
#create new variable of lag range,
lts_metadf_range <- lts_metadf
lts_LAGmin <- min(lts_metadf_range$theLAG) #define LAGmin
lts_LAGmax <- max(lts_metadf_range$theLAG) #define LAGmax

lts_metadf_range$lag_range <-
  ifelse(lts_metadf_range$theLAG >= lts_LAGmin & lts_metadf_range$theLAG < 0,"negativeLAG", # >= so max OR min not assigned as zero lag
         ifelse(lts_metadf_range$theLAG > 0 & lts_metadf_range$theLAG <= lts_LAGmax, "positiveLAG","zeroLAG"))


# lts_metadf_range_portion <- lts_metadf_range %>%
#   filter()


lts_summ_lag_range # corrs per lag, with lag annotated as before and after

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
# lts_summ_lag_range

#a portion of group by unique observation, prior, post or zero, and get summary stats
lts_summ_lag_range_portion <- lts_metadf_range %>%
  # dplyr::filter(lag_range != "zeroLAG") %>% #consider all lags (including zero)
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

colnames(lts_summ_lag_range_portion) <- gsub(colnames(lts_summ_lag_range_portion), pattern = "_lts_portion", replacement = paste0("_maxLag_",as.character(.lts_portion)))



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

lts_summ_lag_range_diffed_portion

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

vector_indexOfDoubleID <- do.call(c,lts_indexOfDoubleID)
vector_indexOfDoubleID



#join double list first
lts_singletonSummary_doubleKey <-
  Reduce(function(dtf1, dtf2) merge(dtf1, dtf2,
  by = c(lts$lts_uniqueID_colname, "lag_range"), all.x = TRUE),
  lts_proto_summ_stats_list[vector_indexOfDoubleID])


lts_singletonSummary_doubleKey

#add merged lists to a new list, and append the unmerged frames to this list
lts_sum_stats_list <-
  list(lts_singletonSummary_doubleKey)
lts_sum_stats_list <-append(lts_sum_stats_list,
    lts_proto_summ_stats_list[!vector_indexOfDoubleID]) #use inverted vector of columns

#join double list first
lts_singletonSummary <-
  Reduce(function(dtf1, dtf2) merge(dtf1, dtf2,
  by = c(lts$lts_uniqueID_colname), all.x = TRUE),
   lts_sum_stats_list)


lts_singletonSummary

###NOW JOIN categorical variables and Make group summary stats
unq_compareBy <- unique(.lts_variables$lts_data[, c(.lts_variables$lts_compare_by, .lts_variables$lts_uniqueID_colname)])

lts_singletonSummary_metadata <- merge(lts_singletonSummary, unq_compareBy, by = c(.lts_variables$lts_uniqueID_colname))  #categorical variables


###OUTPUT ONE
lts_singletonSummary_metadata


# lts_sum_stats_list
#
# lts_proto_summ_stats_list #multiply by minus 1 to select the
#
# lts_singletonSummary <- lts_singletonSummary_doubleKey <-
#   Reduce(function(dtf1, dtf2) merge(dtf1, dtf2,
#   by = c(lts$lts_uniqueID_colname), all.x = TRUE),
#   lts_summ_stats_list[vector_indexOfDoubleID])
#
# lts_singletonSummary
# lts_singletonSummary
# # lts_singletonSummary <- lts_summ_stats_list %>%
# #   Reduce(function(dtf1,dtf2) dplyr::left_join(dtf1,dtf2,by=c(lts$lts_uniqueID_colname, "lag_range")), .)
#
#
#

#Now make each of the summary stats as before, but at the grouped by categorical level


#Calculate DIFFERENCED CCFs
#calculate rates of change in correlation between lags (how quickly does correlation change)
#rate of change in  correlation as a function of
lts_differenced_summ_key_num_portion <- lts_metadf %>%
  dplyr::group_by(key_num) %>%
  dplyr::mutate_at("theCCF", ~ . - dplyr::lag(., n = 1, default = NA),.keep = "all" )
lts_differenced_summ_key_num_portion

###ONE_statistics on entire CCF per observation

# calculate summary statistics on CCF of individual unique observations
# and for a range around lag 0

#Here no more individuals (key num replaced by categoricals)
#for summaries , take summary of individual summaries.
#can have categorical per lag
#can  have categorical overall
#group by categories




### TWO SUMMARY STATISTICS BY CATEGORICAL GROUPS#####

##summary stats derived from catGroup lags




colnames(lts_catGroups_perLAG_summ_key_num_portion) <- gsub(colnames(lts_catGroups_perLAG_summ_key_num_portion), pattern = "_lts_portion", replacement = paste0("_maxLag_",as.character(.lts_portion)))

#summarise by LAG, and by categorical variable
lts_catGroups_summ_key_num <- lts_metadf %>%
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
lts_catGroups_summ_key_num_portion <- lts_metadf %>%
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

colnames(lts_catGroups_summ_key_num_portion) <- gsub(colnames(lts_catGroups_summ_key_num_portion), pattern = "_lts_portion", replacement = paste0("_maxLag_",as.character(.lts_portion)))




# gsub(colnames(.), pattern = "_lts_portion", replacement = as.character(.lts_portion))
#update colname to show the portion of max lag that the summary statistic appies to
###TWO_ statistics caclulated for negative and positive lags per observation
## and for a range around lag 0
#calculate summary statistics separately for pre and post lag
#create labels for lag range
#create new variable of lag range,

# lts_metadf_range <- lts_metadf
# lts_LAGmin <- min(lts_metadf_range$theLAG) #define LAGmin
# lts_LAGmax <- max(lts_metadf_range$theLAG) #define LAGmax
#
# lts_metadf_range$lag_range <-
#   ifelse(lts_metadf_range$theLAG >= lts_LAGmin & lts_metadf_range$theLAG < 0,"negativeLAG", # >= so max OR min not assigned as zero lag
#          ifelse(lts_metadf_range$theLAG > 0 & lts_metadf_range$theLAG <= lts_LAGmax, "positiveLAG","zeroLAG"))


# lts_metadf_range_portion <- lts_metadf_range %>%
#   filter()


 # corrs per lag, with lag annotated as before and after

#group by unique observation, prior, post or zero, and get summary stats
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
lts_catGroups_summ_lag_range

#a portion of group by unique observation, prior, post or zero, and get summary stats
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

colnames(lts_catGroups_summ_lag_range_portion) <- gsub(colnames(lts_catGroups_summ_lag_range_portion), pattern = "_lts_portion", replacement = paste0("_maxLag_",as.character(.lts_portion)))


# notes on across: https://stackoverflow.com/questions/64188671/renaming-multiple-columns-with-dplyr-renameacross
# calcualte diff between positive and negative lags, within each observation
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

lts_catGroups_summ_lag_range_diffed

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

lts_catGroups_summ_lag_range_diffed_portion


#make a list of summary statistics for CATEGORICAL GROUPED SUMMARIES
lts_proto_catGroup_summ_stats_list <-
  list(lts_catGroups_summ_key_num,
       lts_catGroups_summ_key_num_portion,
       lts_catGroups_summ_lag_range,
       lts_catGroups_summ_lag_range_portion,
       lts_catGroups_summ_lag_range_diffed,
       lts_catGroups_summ_lag_range_diffed_portion)

  # list(lts_summ_key_num,
  #      lts_summ_key_num_portion,
  #      lts_summ_lag_range,
  #      lts_summ_lag_range_portion,
  #      lts_summ_lag_range_diffed,
  #      lts_summ_lag_range_diffed_portion)

lts_catGroup_indexOfDoubleID <- vector("list",length(lts_proto_catGroup_summ_stats_list))
element <- 1
for(sums in lts_proto_catGroup_summ_stats_list){
  lts_result_doubleID <- ("lag_range" %in% colnames(sums))
  print(lts_result_doubleID)
  lts_catGroup_indexOfDoubleID[[element]] <- lts_result_doubleID
  element <- element+1
}

vector_catGroup_indexOfDoubleID <- do.call(c,lts_catGroup_indexOfDoubleID)
vector_catGroup_indexOfDoubleID



#join double list first
lts_catGroup_Summary_doubleKey <-
  Reduce(function(dtf1, dtf2) merge(dtf1, dtf2,
                                    by = c(.lts_variables$lts_compare_by, "lag_range"), all.x = TRUE), #join by lag range and categorical variables
         lts_proto_catGroup_summ_stats_list[vector_catGroup_indexOfDoubleID])

lts_catGroup_Summary_doubleKey



#add merged lists to a new list, and append the unmerged frames to this list
lts_catGroup_sum_stats_list <-
  list(lts_catGroup_Summary_doubleKey)

lts_catGroup_sum_stats_list <-append(lts_catGroup_sum_stats_list,
                                     lts_proto_catGroup_summ_stats_list[!vector_catGroup_indexOfDoubleID]) #use inverted vector of columns

#join double list first
##SECOND OUTPUT
lts_catGroupSummary <-
  Reduce(function(dtf1, dtf2) merge(dtf1, dtf2,
                                    by = c(.lts_variables$lts_compare_by), all.x = TRUE),
         lts_catGroup_sum_stats_list)


lts_catGroupSummary

###NOW JOIN categorical variables and Make group summary stats
# unq_compareBy <- unique(.lts_variables$lts_data[, c(.lts_variables$lts_compare_by, .lts_variables$lts_uniqueID_colname)])

# lts_singletonSummary_metadata <- merge(lts_singletonSummary, unq_compareBy, by = c(.lts_variables$lts_uniqueID_colname))  #categorical variables
#
#
# lts_singletonSummary_metadata





###THREE get PER LAG summary stats for all of the data
####UNGROUPED AND GROUPED BY CATEGORY


lts_summ_perLAG <- lts_metadf %>%
  dplyr::group_by(theLAG) %>%
  dplyr::summarise(mean_corr = mean(theCCF),
                   max_corr = max(theCCF),
                   min_corr = min(theCCF),
                   var_corr = var(theCCF),
                   sd_corr = sd(theCCF),
                   median_corr = median(theCCF))
lts_summ_perLAG

#define mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#ALERT
#get Lag with the mode (most frequent) max correlation for the entire dataset
modeMaxCorrLAG <- lts_summ_perLAG %>%
  dplyr::select(theLAG, mean_corr)%>%
  dplyr::top_n(1, mean_corr) %>%
  unique() %>%
  dplyr::pull(theLAG) %>%
  Mode()



head(lts_summ_perLAG,20)

#get mean correlation across all of data set at lag with max mean correlation
mean_corr_at_modeMaxCorrLAG <- lts_summ_perLAG %>%  #calculate mean Corr at Lag with mode max correlation
  dplyr::filter(theLAG == modeMaxCorrLAG) %>%
  # dplyr::group_by(dplyr::across(.lts_variables$lts_compare_by)) %>% #group by vector of cluster groups
  dplyr::summarise(meanCorrAtModeMaxLAG = mean(mean_corr, na.rm = TRUE)) #summarise the mean at lag 0



# #get Lag with the mode (most frequent) max correlation for the entire dataset
# lts_catGroups_summ_mode_max_lag <- join_ccflist_wMetaD %>%
#   dplyr::select(.lts_variables$lts_compare_by, theLAG, meanCorrPerLag)%>%
#   dplyr::group_by(dplyr::across(.lts_variables$lts_compare_by)) %>%
#   dplyr::top_n(1, meanCorrPerLag) %>%
#   unique() %>%
#   dplyr::pull(theLAG) %>%
#   Mode()




### PER LAG BUT ALSO CATEGORICAL GROUPS####

#summarise by LAG, and by categorical variable
lts_catGroups_summ_perLAG <- lts_metadf %>%
  dplyr::group_by(!!rlang::sym(.lts_compare_by[[1]]),
                  !!rlang::sym(.lts_compare_by[[2]]),
                  theLAG) %>%
  dplyr::summarise(mean_corr = mean(theCCF),
                   max_corr = max(theCCF),
                   min_corr = min(theCCF),
                   var_corr = var(theCCF),
                   sd_corr = sd(theCCF),
                   median_corr = median(theCCF))


#ALERT
#get Lag with the mode (most frequent) max correlation for the entire dataset
#starting with already summarised mean Corr per lag,
#group by categorical variables and take the top lag (there can only be top because of the grouping)
catGroups_maxLAG_and_max_meanCorr_perLAG <- lts_catGroups_summ_perLAG %>%
  dplyr::select(!!rlang::sym(.lts_compare_by[[1]]),
                !!rlang::sym(.lts_compare_by[[2]]),
                theLAG, mean_corr)%>%
  dplyr::group_by(!!rlang::sym(.lts_compare_by[[1]]),
                  !!rlang::sym(.lts_compare_by[[2]])) %>%
  dplyr::top_n(1, mean_corr)


# %>%
#  dplyr::summarise(mode_max_mean_Lag = Mode(theLAG),)


unique() %>%
  dplyr::pull(theLAG) %>%
  Mode()
catGroups_modeMaxCorrLAG

# unique() %>%
  # dplyr::pull(theLAG) %>%
  # Mode()

catGroups_modeMaxCorrLAG

head(lts_summ_perLAG,20)

#get mean correlation across all of data set at lag with max mean correlation
mean_corr_at_modeMaxCorrLAG <- lts_summ_perLAG %>%  #calculate mean Corr at Lag with mode max correlation
  dplyr::filter(theLAG == modeMaxCorrLAG) %>%
  # dplyr::group_by(dplyr::across(.lts_variables$lts_compare_by)) %>% #group by vector of cluster groups
  dplyr::summarise(meanCorrAtModeMaxLAG = mean(mean_corr, na.rm = TRUE)) #summarise the mean at lag 0




#calculate summary statiscs on CCF of individual unique observations
#over specified portion of the max lag (.lts_portion)
lts_catGroups_perLAG_summ_key_num_portion <- lts_metadf %>%
  dplyr::filter(
    (theLAG < (!!.lts_portion)*max(theLAG)) &
      (theLAG > (!!.lts_portion)*min(theLAG))
  ) %>%
  dplyr::group_by(!!rlang::sym(.lts_compare_by[[1]]),
                  !!rlang::sym(.lts_compare_by[[2]]),
                  theLAG) %>%
  dplyr::summarise(mean_corr_lts_portion = mean(theCCF),
                   max_corr_lts_portion = max(theCCF),
                   min_corr_lts_portion = min(theCCF),
                   var_corr_lts_portion = var(theCCF),
                   sd_corr_lts_portion = sd(theCCF),
                   median_corr_lts_portion = median(theCCF)) #%>% #add abs max lag to name in place of portion
# gsub(colnames(.), pattern = "_lts_portion", replacement = as.character(.lts_portion))






###OUTPUT FROM LAG STATS

#make a list of summary statistics for CATEGORICAL GROUPED SUMMARIES
lts_proto_catGroup_summ_stats_list <-
  list(lts_catGroups_summ_key_num,
       lts_catGroups_summ_key_num_portion,
       lts_catGroups_summ_lag_range,
       lts_catGroups_summ_lag_range_portion,
       lts_catGroups_summ_lag_range_diffed,
       lts_catGroups_summ_lag_range_diffed_portion)

# list(lts_summ_key_num,
#      lts_summ_key_num_portion,
#      lts_summ_lag_range,
#      lts_summ_lag_range_portion,
#      lts_summ_lag_range_diffed,
#      lts_summ_lag_range_diffed_portion)

lts_catGroup_indexOfDoubleID <- vector("list",length(lts_proto_catGroup_summ_stats_list))
element <- 1
for(sums in lts_proto_catGroup_summ_stats_list){
  lts_result_doubleID <- ("lag_range" %in% colnames(sums))
  print(lts_result_doubleID)
  lts_catGroup_indexOfDoubleID[[element]] <- lts_result_doubleID
  element <- element+1
}

vector_catGroup_indexOfDoubleID <- do.call(c,lts_catGroup_indexOfDoubleID)
vector_catGroup_indexOfDoubleID



#join double list first
lts_catGroup_Summary_doubleKey <-
  Reduce(function(dtf1, dtf2) merge(dtf1, dtf2,
                                    by = c(.lts_variables$lts_compare_by, "lag_range"), all.x = TRUE), #join by lag range and categorical variables
         lts_proto_catGroup_summ_stats_list[vector_catGroup_indexOfDoubleID])

lts_catGroup_Summary_doubleKey



#add merged lists to a new list, and append the unmerged frames to this list
lts_catGroup_sum_stats_list <-
  list(lts_catGroup_Summary_doubleKey)

lts_catGroup_sum_stats_list <-append(lts_catGroup_sum_stats_list,
                                     lts_proto_catGroup_summ_stats_list[!vector_catGroup_indexOfDoubleID]) #use inverted vector of columns

#join double list first
##SECOND OUTPUT
lts_catGroupSummary <-
  Reduce(function(dtf1, dtf2) merge(dtf1, dtf2,
                                    by = c(.lts_variables$lts_compare_by), all.x = TRUE),
         lts_catGroup_sum_stats_list)




#FOUR get lag with max correlation per time track



#FIVe get lag with most frequently occuring max correlation per categorical variables


#six calculate average correlation at the lag with mode max correlation



#make a list of catGroup summary statistics
lts_proto_catGroups_lag_summ_stats_list <-
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

vector_indexOfDoubleID <- do.call(c,lts_indexOfDoubleID)
vector_indexOfDoubleID



#join double list first
lts_singletonSummary_doubleKey <-
  Reduce(function(dtf1, dtf2) merge(dtf1, dtf2,
                                    by = c(lts$lts_uniqueID_colname, "lag_range"), all.x = TRUE),
         lts_proto_summ_stats_list[vector_indexOfDoubleID])


lts_singletonSummary_doubleKey

#add merged lists to a new list, and append the unmerged frames to this list
lts_sum_stats_list <-
  list(lts_singletonSummary_doubleKey)
lts_sum_stats_list <-append(lts_sum_stats_list,
                            lts_proto_summ_stats_list[!vector_indexOfDoubleID]) #use inverted vector of columns

#join double list first
lts_singletonSummary <-
  Reduce(function(dtf1, dtf2) merge(dtf1, dtf2,
                                    by = c(lts$lts_uniqueID_colname), all.x = TRUE),
         lts_sum_stats_list)



#
# View(lts_singletonSummary)
# unq_single <- unique(lts_singletonSummary)
#
# View(unq_single)
#
# # mutate(across(where(is.numeric)), ~ . - dplyr::lag(., n = 1, default = NA),.keep = "all" )
#
#
# # courtesy of  https://stackoverflow.com/questions/14846547/calculate-difference-between-values-in-consecutive-rows-by-group
#
# lts_summ_lag_range_portion <- lts_metadf_range %>%
#   dplyr::filter(lag_range != "zeroLAG") %>% #only consider lags that are not simultaneous measures
#   dplyr::group_by(key_num, lag_range) %>%
#   dplyr::filter(
#     (theLAG < (!!.lts_portion)*max(theLAG)) & #filter out lags that are not within a certain portion of range of lags
#       (theLAG > (!!.lts_portion)*min(theLAG))
#   ) %>%
#   dplyr::summarise(mean_corr_by_lag_range_lts_portion = mean(theCCF),
#                    max_corr_by_lag_range_lts_portion = max(theCCF),
#                    min_corr_by_lag_range_lts_portion = min(theCCF),
#                    var_corr_by_lag_range_lts_portion = var(theCCF),
#                    sd_corr_lag_by_range_lts_portion = sd(theCCF),
#                    median_corr_by_lag_range_lts_portion = median(theCCF))
#
# ###Three_statistcs calculated based on comparisons between positive and negative lags
#
#
#
#
#
#
#
#
#
# Arranged_lts_clusterCCFs$lagRange <- # nested ifelse to set ranges of lags
#   ifelse(Arranged_lts_clusterCCFs$theLAG >LAGmin & Arranged_lts_clusterCCFs$theLAG <0 , "negativeLAG",
#          ifelse(Arranged_lts_clusterCCFs$theLAG >0 & Arranged_lts_clusterCCFs$theLAG <LAGmax, "positiveLAG","zeroLAG"))
#
#
#
#
# #define pre and post ranges
#
#
# lts_summ_key_num <- lts_metadf %>%
#   dplyr::group_by(key_num) %>%
#   dplyr::summarise(mean_corr = mean(theCCF),
#                    max_corr = max(theCCF),
#                    min_corr = min(theCCF),
#                    var_corr = var(theCCF),
#                    sd_corr = sd(theCCF),
#                    median_corr = median(theCCF))
#
#
# #find lag associated with min and max
#
#
# #max corr DONE
# #min corr DONE
# #variance in corr DONE
# #average corr DONE
# #Lag with max corr
# #difference in pre and post lags (asymmetry)
#
# # As above but at half max lag (this half max can be a paramater)
# # max corr
# # min corr
# # variance in half max lag corr
# # lag with max corr
# # difference in pre and post lags (asymmetry)
#
#
#
# #THEN group by categories and summaries
#
#
#
#
# env
#
# .lts_ccfWithMetaData = lts_ccfWithMetaData_compareBy
# .lts_variables = NULL


