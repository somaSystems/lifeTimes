# clusterCCFs !

#TODO: in future can make summary fo cluster by different options, eg. cluster by max lag rather than lag 0, or cluster by other thing
# getwd()
#  setwd(paste0(getwd(),"/R"))
# # Cluster cross correlations by coefficient at lag 0
# outputCCF <- timesChain() #getouput of all steps up until CCF

# head(join_outputCCFdata)
# colnames(join_outputCCFdata)
# join_outputCCFdata$anCCF_ACF
# Visualise data by organizing into a heatmap through correlation at lag zero
#

#Resources on clustering and dendrograms
##https://stackoverflow.com/questions/42047896/joining-a-dendrogram-and-a-heatmap
#https://yulab-smu.top/treedata-book/chapter12.html
#https://ggplot2-book.org/facet.html


#ggokit dendrograms and heatmaps
#https://jcoliver.github.io/learn-r/008-ggplot-dendrograms-and-heatmaps.html


clusterCCFs <- function(outputCCFdata_withMetaData, clustGroups = defaultClustGroups){
library(rlang)
library(tidyr)

defaultClustGroups <- c("Treatment","an_CCF_Feature")

mean_ccfLAGzero <- outputCCFdata_withMetaData %>%
  filter(anCCF_LAG == 0)%>% #filter lag is 0
  group_by_at(defaultClustGroups)%>% #group by vector of cluster groups
  summarise(meanLAGzero = mean(anCCF_ACF, na.rm = TRUE)) #summarise the mean at lag 0

sum_outputCCFdata_withMetaData <- left_join(outputCCFdata_withMetaData, mean_ccfLAGzero, by = c("Treatment", "an_CCF_Feature")) # join mean lag to input dataframe for clustering

#remove features that are same for cell and nucleus #Todo, remove unused factor levels eg. coords
identicalMeasuresAt0 <- sum_outputCCFdata_withMetaData[sum_outputCCFdata_withMetaData$meanLAGzero ==1, ]
unq_identicalFeatures <- c(unique(identicalMeasuresAt0$an_CCF_Feature))
print(paste("Warning: feature ",unq_identicalFeatures,"has correlation 1 at lag zero and will be removed"))
# subset_sum_outputCCFdata_withMetaData <- sum_outputCCFdata_withMetaData[sum_outputCCFdata_withMetaData$meanLAGzero !=1, ] #subset to remove perfectly correlated features
# unique(subset_sum_outputCCFdata_withMetaData$an_CCF_Feature)
sum_outputCCFdata_withMetaData
subset_sum_outputCCFdata_withMetaData <- sum_outputCCFdata_withMetaData[!grepl(paste(unq_identicalFeatures, collapse="|"), sum_outputCCFdata_withMetaData$an_CCF_Feature),] #https://stackoverflow.com/questions/38724690/r-filter-rows-that-contain-a-string-from-a-vector
subset_sum_outputCCFdata_withMetaData
#create a matrix of mean correlation at lag zero in preparation to cluster cell and nuclear correlations
unq_subset_sum_join_outputCCFdata<- unique(subset_sum_outputCCFdata_withMetaData) # get unique value for each cell and lag and feature etc (some of these may be duplicated if plate or metadata is duplicated)

subset_sum_join_outputCCFdata <- subset_sum_outputCCFdata_withMetaData #update this later so that no assignment needed in code below

m_wide_subset_sum_join_outputCCFdata <- subset_sum_join_outputCCFdata %>% #make wider, put treatment as colnames, put values as lag0
  select(meanLAGzero,an_CCF_Feature,Treatment)%>%
  unique()%>%
  pivot_wider(
    id_col = c("meanLAGzero","an_CCF_Feature","Treatment"),
    names_from = "Treatment",
    values_from = "meanLAGzero")

mCCF_lagZero <- as.matrix(m_wide_subset_sum_join_outputCCFdata[-1]) #make a numerical only matrix of mean correlation at lag zero, by removing first column
rownames(mCCF_lagZero) <-  m_wide_subset_sum_join_outputCCFdata$an_CCF_Feature # add rownames to matrix

#cluster matrix
row.order <- hclust(dist(mCCF_lagZero))$order # get row order from clustered matrix and set this as a  variable
col.order <- hclust(dist(t(mCCF_lagZero)))$order # get column order from clustered matrix and set this as a variable
mCCF_lagZero[row.order, col.order] #display matrix organised by rows and columns

#make dendrogram
treatmentDendrogram <<- as.dendrogram(hclust(dist(t(mCCF_lagZero)))) #make dendrogram

library("ggdendro")
draw_treatmentDendrogram <<- ggdendrogram(treatmentDendrogram)
draw_treatmentDendrogram
# draw_treatmentDendrogram
# row.order
# col.order

# get list of rows with names for ordering (order the factor this way)
featureOrder <- m_wide_subset_sum_join_outputCCFdata$an_CCF_Feature # make the features a variable called feature order
featureOrder # look at current feature order (this is order will be plotted in ggplot)
newFeatureOrder <- featureOrder[row.order] #make a new or desired feature order based on the row order
featureOrder <- factor(featureOrder, levels = newFeatureOrder) # reorder the factor levels of the feature to match the clsutering
featureOrder

# Treatment orders (do same process for factors but for treatments)
treatmentOrder <- colnames(mCCF_lagZero)
newTreatmentOrder <- treatmentOrder[col.order]
treatmentOrder <- factor(treatmentOrder, levels = newTreatmentOrder)
treatmentOrder
#update main dataframe (update the factor levels for Treatments and Features, to be based on clustering)
subset_sum_join_outputCCFdata$Treatment <- factor(subset_sum_join_outputCCFdata$Treatment, levels =newTreatmentOrder)
subset_sum_join_outputCCFdata$an_CCF_Feature  <- factor(subset_sum_join_outputCCFdata$an_CCF_Feature, levels = newFeatureOrder)

df_clusteredZeroLag_withMetada <<- subset_sum_join_outputCCFdata

return(df_clusteredZeroLag_withMetada) #returns data with factor levels organised by clustering #in future could include distance metrics
}


