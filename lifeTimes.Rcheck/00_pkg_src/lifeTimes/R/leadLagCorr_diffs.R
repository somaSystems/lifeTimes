# leadLagCorr_diffs

# function to calculate median difference (asymmetry) in the mean of lead and lagging time points for different variables
#TODO, make summaries of pre and post lags medians, rather than means

leadLagCorr_diffs <- function(instanceOf_outputCCFdata_withMetaData){

# funInternal_outputCCFdata_withMetaData <- example_outputCCFdata_withMetaData

library(tidyr)
library(dplyr)

Arranged_outputCCFdata_withMetaData <- instanceOf_outputCCFdata_withMetaData %>% #arrange input dataframe
arrange(an_CCF_ObjectID,an_CCF_Feature, Treatment,anCCF_LAG,)%>%
group_by(an_CCF_ObjectID, an_CCF_Feature,Treatment)



#create new variable of lag range,
Arranged_outputCCFdata_withMetaData$lagRange <- # nested ifelse to set ranges of lags
ifelse(Arranged_outputCCFdata_withMetaData$anCCF_LAG >-15 & Arranged_outputCCFdata_withMetaData$anCCF_LAG <0 , "negativeLAG",
ifelse(Arranged_outputCCFdata_withMetaData$anCCF_LAG >0 & Arranged_outputCCFdata_withMetaData$anCCF_LAG <15, "positiveLAG","zeroLAG"))


meanLagRange_outputCCFdata_withMetaData <- Arranged_outputCCFdata_withMetaData %>% # Organise by Cell ID, Feature, Treatment, and Lag (timepoint),
arrange(an_CCF_ObjectID,an_CCF_Feature, Treatment,anCCF_LAG)%>%
group_by(an_CCF_ObjectID, an_CCF_Feature,Treatment, lagRange) %>%
dplyr::summarise(meanACFforLAG = mean(anCCF_ACF, na.rm = TRUE)) # calculate average correlations in lags (either side of zero)

#make wider, taking names from lag range and values from mean lag
#This gives a table of every, cell, with average prior, post and zero lags calculated
wider_meanLagRange_join_outputCCFdata <-
pivot_wider(meanLagRange_outputCCFdata_withMetaData,
id_cols = c("an_CCF_ObjectID","an_CCF_Feature","Treatment","meanACFforLAG","lagRange"),
names_from = "lagRange",
values_from = "meanACFforLAG")

wider_meanLagRange_join_outputCCFdata$meanPrior_meanPost_diff <- #calculate difference in negative(prior) and positive(post) time lags
wider_meanLagRange_join_outputCCFdata$negativeLAG -
wider_meanLagRange_join_outputCCFdata$positiveLAG

join_meanLagRange_outputCCFdata_withMetaData <- left_join( #join prior to post ratio back to dataframe
Arranged_outputCCFdata_withMetaData,
wider_meanLagRange_join_outputCCFdata[,],
by = c("an_CCF_ObjectID","an_CCF_Feature","Treatment"))

dim(Arranged_outputCCFdata_withMetaData) # wider_meanLagRange_join_outputCCFdata
dim(join_meanLagRange_outputCCFdata_withMetaData)

summaryOfMedianDifferencePriorAndPost<- #get overall feature mean for clustering
join_meanLagRange_outputCCFdata_withMetaData %>%
group_by(an_CCF_Feature,Treatment)%>%
summarise(medianPrePostPerTF = median(meanPrior_meanPost_diff))

join_medianDiff_meanLagRange_outputCCFdata_withMetaData <<- #Join summary back to original dataframe, make a global variable
left_join(join_meanLagRange_outputCCFdata_withMetaData,
summaryOfMedianDifferencePriorAndPost,
by = c("an_CCF_Feature","Treatment"))

return(join_medianDiff_meanLagRange_outputCCFdata_withMetaData)
}
