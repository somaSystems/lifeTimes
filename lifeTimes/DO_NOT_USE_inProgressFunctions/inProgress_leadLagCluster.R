#leadLagCluster !

#Function to cluster contitions (eg. treatments) and variables (eg. feature measurements)
#by difference in past vs future lags


leadLagCluster <- function(join_medianDiff_meanLagRange_outputCCFdata_withMetaData, removeFeatureByname){
library(ggplot2)
library(viridis)
# join_meanLagRange_join_outputCCFdata <- join_medianDiff_meanLagRange_outputCCFdata_withMetaData

#subset to remove zero lag
subset_meanLagRange_join_outputCCFdata <-
join_medianDiff_meanLagRange_outputCCFdata_withMetaData[join_medianDiff_meanLagRange_outputCCFdata_withMetaData$lagRange != "zeroLAG" ,]

#remove feature by name (helps for eliminating NA features etc)
diffIn_Pre_vs_Postcorrelation <-  subset_meanLagRange_join_outputCCFdata %>%
filter(!grepl(paste0(removeByname, collapse = "|"), an_CCF_Feature))

#remove feature if has NA
rmna_diffIn_Pre_vs_Postcorrelation <-  diffIn_Pre_vs_Postcorrelation %>%
filter(!is.na(medianPrePostPerTF))


head(rmna_diffIn_Pre_vs_Postcorrelation)
colnames(rmna_diffIn_Pre_vs_Postcorrelation)
# unique(diffIn_Pre_vs_Postcorrelation$an_CCF_Feature)

filteredForM_diffIn_Pre_vs_Postcorrelation<- rmna_diffIn_Pre_vs_Postcorrelation %>%
ungroup()%>%
select(an_CCF_Feature,Treatment,medianPrePostPerTF)
unq_filteredForM_diffIn_Pre_vs_Postcorrelation <- unique(filteredForM_diffIn_Pre_vs_Postcorrelation)
# filteredForM_diffIn_Pre_vs_Postcorrelation
# dim(filteredForM_diffIn_Pre_vs_Postcorrelation)

wide_filteredForM_diffIn_Pre_vs_Postcorrelation <- pivot_wider(
  unq_filteredForM_diffIn_Pre_vs_Postcorrelation,
  names_from = Treatment,
  values_from = medianPrePostPerTF)


mw_prePost <- as.matrix(wide_filteredForM_diffIn_Pre_vs_Postcorrelation[-1])
rownames(mw_prePost) <- wide_filteredForM_diffIn_Pre_vs_Postcorrelation$an_CCF_Feature

library(ComplexHeatmap)

library(circlize)
col_fun = colorRamp2(c(min(mw_prePost), 0,max(mw_prePost)), c("steelblue4", "white", "gold"))
col_fun(seq(-10, 10))
#
# set.seed(235634634)
# plot("hello")
return(Heatmap(mw_prePost, col = col_fun))
}

join_medianDiff_meanLagRange_outputCCFdata_withMetaData<- leadLagCorr_diffs(outputCCFdata_withMetaData)
removeFeatureByname <-  "nProtrusions"
leadLagCluster(join_medianDiff_meanLagRange_outputCCFdata_withMetaData, removeFeatureByname)
