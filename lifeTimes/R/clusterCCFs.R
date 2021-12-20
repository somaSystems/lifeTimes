# clusterCCFs !

#TODO: in future can make summary fo cluster by different options, eg. cluster by max lag rather than lag 0, or cluster by other thing

# Cluster cross correlations by coefficient at lag 0
outputCCF <- timesChain() #getouput of all steps up until CCF

# head(join_outputCCFdata)
# colnames(join_outputCCFdata)
# join_outputCCFdata$anCCF_ACF
# Visualise data by organizing into a heatmap through correlation at lag zero

outputCCFdata_withMetaData <- metaData_ccf_join(outputCCF) #run metaData_ccf_join on output of CCF ans save results

defaultClustGroups <- c("Treatment","an_CCF_Feature")

clusterCCFs <- function(outputCCFdata_withMetaData, clustGroups = defaultClustGroups){
library(rlang)
library(tidyr)

mean_ccfLAGzero <- outputCCFdata_withMetaData %>%
  filter(anCCF_LAG == 0)%>% #filter lag is 0
  group_by_at(defaultClustGroups)%>% #group by vector of cluster groups
  summarise(meanLAGzero = mean(anCCF_ACF, na.rm = TRUE)) #summarise the mean at lag 0

sum_outputCCFdata_withMetaData <- left_join(outputCCFdata_withMetaData, mean_ccfLAGzero, by = c("Treatment", "an_CCF_Feature")) # join mean lag to input dataframe for clustering

#remove features that are same for cell and nucleus #Todo, remove unused factor levels eg. coords
identicalMeasuresAt0 <- sum_outputCCFdata_withMetaData[sum_outputCCFdata_withMetaData$meanLAGzero ==1, ]
unq_identicalFeatures <- c(unique(identicalColumns$an_CCF_Feature))
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
    values_from = "meanLAGzero"
  )

m_wide_subset_sum_join_outputCCFdata

#resume from here

str(m_wide_subset_sum_join_outputCCFdata)


#cluster data by mean correlation at lag zero
```{r}

#add rownames and remove first column to make numeric matrix
rnames_m_wide_subset_sum_join_outputCCFdata <- m_wide_subset_sum_join_outputCCFdata
rownames(rnames_m_wide_subset_sum_join_outputCCFdata) <- m_wide_subset_sum_join_outputCCFdata$an_CCF_Feature

mCCF_lagZero <- as.matrix(rnames_m_wide_subset_sum_join_outputCCFdata[-1]) #make a matrix
rownames(mCCF_lagZero) <-  m_wide_subset_sum_join_outputCCFdata$an_CCF_Feature # add rownames to matrix

row.order <- hclust(dist(mCCF_lagZero))$order # get row order from clustered matrix and set this as a  variable
col.order <- hclust(dist(t(mCCF_lagZero)))$order # get column order from clustered matrix and set this as a variable

row.order
col.order

mCCF_lagZero[row.order, col.order]

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

#update main dataframe (update the factor levels for Treatments and Features, to be based on clustering)
subset_sum_join_outputCCFdata$Treatment <- factor(subset_sum_join_outputCCFdata$Treatment, levels =newTreatmentOrder)
subset_sum_join_outputCCFdata$an_CCF_Feature  <- factor(subset_sum_join_outputCCFdata$an_CCF_Feature, levels = newFeatureOrder)



#plot unclustered and clustered data
```{r}
# library(ggplot2)
ggplot(subset_sum_join_outputCCFdata, aes(x =anCCF_LAG, y = anCCF_ACF, group = an_CCF_ObjectID))+
  geom_line(alpha = 0.1)+
  facet_wrap(~an_CCF_Feature)+
  stat_summary(aes(y = anCCF_ACF,group=1), fun.y=mean, colour="darkorange", geom="line",group=1, size = 1)+
  theme_classic()

# join_outputCCFdata
ggplot(subset_sum_join_outputCCFdata, aes(x =anCCF_LAG, y = anCCF_ACF, group = an_CCF_ObjectID, color = Treatment))+
  geom_line(alpha = 0.1)+
  facet_grid( vars(an_CCF_Feature),vars(Treatment) )+
  stat_summary(aes(y = anCCF_ACF,group=1), fun.y=mean, colour="gray20", geom="line",group=1, size = 1)+
  theme_classic()

#Clustered, and coloured by mean, with identicle feature removed
ggplot(subset_sum_join_outputCCFdata[,],
       aes(x =subset_sum_join_outputCCFdata$anCCF_LAG, y = subset_sum_join_outputCCFdata$anCCF_ACF, group = subset_sum_join_outputCCFdata$an_CCF_ObjectID, color = subset_sum_join_outputCCFdata$meanLAGzero))+
  geom_line(alpha = 0.5)+
  scale_color_viridis_c(option ="magma")+
  facet_grid( vars(an_CCF_Feature),vars(Treatment) )+
  stat_summary(aes(y = subset_sum_join_outputCCFdata$anCCF_ACF,group=1), fun.y=mean, colour="gray20", geom="line",group=1, size = 1)+
  theme_classic()

#Clustered and background coloured
ggplot(subset_sum_join_outputCCFdata[,],
       aes(x =subset_sum_join_outputCCFdata$anCCF_LAG, y = subset_sum_join_outputCCFdata$anCCF_ACF, group = subset_sum_join_outputCCFdata$an_CCF_ObjectID))+
  geom_rect(data=subset_sum_join_outputCCFdata, aes(ymin=-1, ymax=1, xmin=-15,
                                                    xmax=15, fill=subset_sum_join_outputCCFdata$meanLAGzero, color = subset_sum_join_outputCCFdata$meanLAGzero), alpha =0.1)+

  geom_line(alpha = 0.5)+
  scale_color_viridis_c(option ="magma")+
  scale_fill_viridis_c(option ="magma")+
  facet_grid( vars(an_CCF_Feature),vars(Treatment) )+
  stat_summary(aes(y = subset_sum_join_outputCCFdata$anCCF_ACF,group=1), fun.y=mean, colour="darkorange", geom="line",group=1, size = 1)+
  theme_classic()

```
