# leadLagCorrelations

# function to calculate difference in the mean of lead and lagging for different variables

#TODO, make summaries of pre and post lags medians, rather than means


#1. Get differnece in all lags

library(tidyr)
library(dplyr)
Arranged_outputCCFdata_withMetaData <- outputCCFdata_withMetaData %>%
   arrange(an_CCF_ObjectID,an_CCF_Feature, Treatment,anCCF_LAG,)%>%
   group_by(an_CCF_ObjectID, an_CCF_Feature,Treatment)

#create new variable of lag range
outputCCFdata_withMetaData$lagRange <- # nested ifelse to set ranges of lags
  ifelse(Arranged_join_outputCCFdata$anCCF_LAG >-15 & Arranged_join_outputCCFdata$anCCF_LAG <0 , "negativeLAG",
  ifelse(Arranged_join_outputCCFdata$anCCF_LAG >0 & Arranged_join_outputCCFdata$anCCF_LAG <15, "positiveLAG","zeroLAG"))

# Organise by Cell ID, Feature, Treatment, and Lag (timepoint),
# calculate average correlations in lags (either side of zero)
meanLagRange_outputCCFdata_withMetaData <- outputCCFdata_withMetaData %>%
  arrange(an_CCF_ObjectID,an_CCF_Feature, Treatment,anCCF_LAG)%>%
  group_by(an_CCF_ObjectID, an_CCF_Feature,Treatment, lagRange) %>%
  dplyr::summarise(meanACFforLAG = mean(anCCF_ACF, na.rm = TRUE))

#make wider, taking names from lag range and values from mean lag
#This gives a table of every, cell, with average prior, post and zero lags calculated
wider_meanLagRange_join_outputCCFdata <-
  pivot_wider(meanLagRange_outputCCFdata_withMetaData,
  id_cols = c("an_CCF_ObjectID","an_CCF_Feature","Treatment","meanACFforLAG","lagRange"),
  names_from = "lagRange",
  values_from = "meanACFforLAG")
# head(wider_meanLagRange_join_outputCCFdata)

#calculate difference in negative(prior) and positive(post) time lags
wider_meanLagRange_join_outputCCFdata$meanPrior_meanPost_diff <-
  wider_meanLagRange_join_outputCCFdata$negativeLAG -
  wider_meanLagRange_join_outputCCFdata$positiveLAG

# head(wider_meanLagRange_join_outputCCFdata)
#join back to main dataframe
#join prior to post ratio back to dataframe
join_meanLagRange_outputCCFdata_withMetaData <- left_join(
  Arranged_outputCCFdata_withMetaData,
  wider_meanLagRange_join_outputCCFdata[,],
  by = c("an_CCF_ObjectID","an_CCF_Feature","Treatment"))

# wider_meanLagRange_join_outputCCFdata
dim(Arranged_outputCCFdata_withMetaData)
dim(join_meanLagRange_outputCCFdata_withMetaData)

#get overall feature mean for clustering
summaryOfMedianDifferencePriorAndPost<-
  join_meanLagRange_outputCCFdata_withMetaData %>%
  group_by(an_CCF_Feature,Treatment)%>%
  summarise(medianPrePostPerTF = median(meanPrior_meanPost_diff))

#Join summary back to original dataframe
join_medianDiff_meanLagRange_outputCCFdata_withMetaData <-
  left_join(join_meanLagRange_outputCCFdata_withMetaData,
            summaryOfMedianDifferencePriorAndPost,
            by = c("an_CCF_Feature","Treatment"))

dim(join_meanLagRange_outputCCFdata_withMetaData)
dim(join_medianDiff_meanLagRange_outputCCFdata_withMetaData)



  # ifelse(Arranged_join_outputCCFdata$anCCF_LAG >-15 & Arranged_join_outputCCFdata$anCCF_LAG <0 , "negativeLAG",
  # ifelse(Arranged_join_outputCCFdata$anCCF_LAG >0 & Arranged_join_outputCCFdata$anCCF_LAG <15, "positiveLAG",
  # ifelse(Arranged_join_outputCCFdata$anCCF_LAG == 0, "zeroLAG", "OutOfLagRange")))

# summary_meandiff_overLAGrange_join_outputCCFdata <- Arranged_join_outputCCFdata %>%
#   group_by(an_CCF_ObjectID, an_CCF_Feature,Treatment,lagRange)%>%
#   dplyr::summarise(meanLAGdiff = mean(anCCF_ACF, na.rm = TRUE),
#                    meanAbsLAGdiff = mean(absCCF_diff, na.rm = TRUE))



#code for differencing data
# diff_join_outputCCFdata <- join_outputCCFdata %>%
#   arrange(an_CCF_ObjectID,an_CCF_Feature, Treatment,anCCF_LAG)%>%
#   group_by(an_CCF_ObjectID, an_CCF_Feature,Treatment) %>%
#   mutate_at("anCCF_ACF", ~ . - dplyr::lag(., n = 1, default = NA),.keep = "all" )
#
# diff_join_outputCCFdata$CCF_diff <- diff_join_outputCCFdata$anCCF_ACF
# #
# # head(testArranger_join_outputCCFdata)
# # head(diff_join_outputCCFdata)
#
# # #create absolute difference measure (getting absolute change in correlation)
# diff_join_outputCCFdata$absCCF_diff <- abs(diff_join_outputCCFdata$CCF_diff)
# abs(diff_join_outputCCFdata$absCCF_diff)
# diff_join_outputCCFdata
#
# #2. get average difference for lags less than 0
#



#doing individual
#NB, could do overall averages and then summarise this

##Possible MISTAKE --> Need to group by lag range?
# diff_join_outputCCFdata

# summary_meandiff_overLAGrange_join_outputCCFdata <- Arranged_join_outputCCFdata %>%
#   group_by(an_CCF_ObjectID, an_CCF_Feature,Treatment,lagRange)%>%
#   dplyr::summarise(meanLAGdiff = mean(anCCF_ACF, na.rm = TRUE),
#                    meanAbsLAGdiff = mean(absCCF_diff, na.rm = TRUE))

# summary_meandiff_overLAGrange_join_outputCCFdata


# colnames(summary_meandiff_overLAGrange_join_outputCCFdata)
# widerDiffs <- pivot_wider(summary_meandiff_overLAGrange_join_outputCCFdata,
#                           id_cols = c("an_CCF_ObjectID","an_CCF_Feature","Treatment","meanAbsLAGdiff","lagRange"),
#                           names_from = "lagRange",
#                           values_from = "meanAbsLAGdiff")
#
#
#
#
#
# widerDiffs$priorToPostRatio <- widerDiffs$negativeLAG/widerDiffs$positiveLAG
# head(widerDiffs)
# #rejoin ratios back to main summary
# widerDiffs[,c(1,2,3,7)]
#
# join_summary_meandiff_overLAGrange_join_outputCCFdata <- left_join(summary_meandiff_overLAGrange_join_outputCCFdata, widerDiffs[,c(1,2,3,7)], by = c("an_CCF_ObjectID","an_CCF_Feature","Treatment"))
#
# dim(summary_meandiff_overLAGrange_join_outputCCFdata)
# dim(join_summary_meandiff_overLAGrange_join_outputCCFdata)
#
#
# ?pivot_wider()
# head(summary_meandiff_overLAGrange_join_outputCCFdata)

#3. Get average difference for lags greater than 0

# -0.1781466	--0.1402122




#look back to original data based on this
#get difference between pre and post lags


#changes on 1 12 21
#
# join_outputCCFdata$lagRange <- ifelse(diff_join_outputCCFdata$anCCF_LAG >-15 & diff_join_outputCCFdata$anCCF_LAG <0 , "negativeLAG",
#                                       ifelse(diff_join_outputCCFdata$anCCF_LAG >0 & diff_join_outputCCFdata$anCCF_LAG <15, "positiveLAG",
#                                              ifelse(diff_join_outputCCFdata$anCCF_LAG == 0, "zeroLAG", "OutOfLagRange")))
#


# # Define ranges of lags around zero
# join_outputCCFdata$lagRange <- ifelse(join_outputCCFdata$anCCF_LAG >-15 & join_outputCCFdata$anCCF_LAG <0 , "negativeLAG",
#                                       ifelse(join_outputCCFdata$anCCF_LAG >0 & join_outputCCFdata$anCCF_LAG <15, "positiveLAG",
#                                              "zeroLAG"))

# library(dplyr)

# join_outputCCFdata$lagRange


# meanLagRange_join_outputCCFdata


# wider_meanLagRange_join_outputCCFdata
# wider_meanLagRange_join_outputCCFdata

# str(wider_meanLagRange_join_outputCCFdata$meanPrior_meanPost_diff)

# wider_meanLagRange_join_outputCCFdata$priorToPostRatio <- wider_meanLagRange_join_outputCCFdata$negativeLAG/wider_meanLagRange_join_outputCCFdata$positiveLAG

# colnames(wider_meanLagRange_join_outputCCFdata)



