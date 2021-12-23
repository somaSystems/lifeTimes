#leadLagClusterPlot !

#Function to cluster contitions (eg. treatments) and variables (eg. feature measurements)
#by difference in past vs future lags

###Toggle these next two lines on and off during testing
# leadLagPlotInput
# join_medianDiff_meanLagRange_outputCCFdata_withMetaData

leadLagClusterPlot <- function(join_medianDiff_meanLagRange_outputCCFdata_withMetaData, removeFeatureByname){


subset_meanLagRange_join_outputCCFdata <-
join_medianDiff_meanLagRange_outputCCFdata_withMetaData[join_medianDiff_meanLagRange_outputCCFdata_withMetaData$lagRange != "zeroLAG" ,]

if(!missing(removeFeatureByname)) {
  subset_meanLagRange_join_outputCCFdata <-  subset_meanLagRange_join_outputCCFdata %>%
      filter(!grepl(paste0(removeByname, collapse = "|"), an_CCF_Feature))}

else {


      # join_meanLagRange_join_outputCCFdata <- join_medianDiff_meanLagRange_outputCCFdata_withMetaData

#subset to remove zero lag

#remove feature by name (helps for eliminating NA features etc)


#remove feature if has NA
rmna_diffIn_Pre_vs_Postcorrelation <-  subset_meanLagRange_join_outputCCFdata %>%
filter(!is.na(medianPrePostPerTF)) # NB: could remove this step

# View(rmna_diffIn_Pre_vs_Postcorrelation)
# head(rmna_diffIn_Pre_vs_Postcorrelation)
# colnames(rmna_diffIn_Pre_vs_Postcorrelation)
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

# https://stackoverflow.com/questions/9505849/r-how-to-get-row-and-column-names-of-the-true-elements-of-a-matrix
hasNA_So_removedFromMatrix <- names(which(rowSums(is.na(mw_prePost)) > 0))
print(paste("Warning:...",hasNA_So_removedFromMatrix,"...had NA, so removed from matrix"))

narm_mw_prePost <- na.omit(mw_prePost)


library(ComplexHeatmap)
library(circlize)
col_fun = colorRamp2(c(min(narm_mw_prePost), 0,max(narm_mw_prePost)), c("steelblue4", "white", "gold"))
col_fun(seq(-10, 10))

Heatmap(narm_mw_prePost, col = col_fun)
return((Heatmap(narm_mw_prePost, col = col_fun)))
}
}
