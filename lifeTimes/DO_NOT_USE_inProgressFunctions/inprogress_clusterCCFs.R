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
treatmentDendrogram <- as.dendrogram(hclust(dist(t(mCCF_lagZero)))) #make dendrogram

library("ggdendro")
draw_treatmentDendrogram <- ggdendrogram(treatmentDendrogram)
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

return(subset_sum_join_outputCCFdata) #returns data with factor levels organised by clustering #in future could include distance metrics
}

clusterPlot <- function(subset_sum_join_outputCCFdata)
#create heatmap ggplot
library(ggplot2)
heatmap_CCF <- ggplot(subset_sum_join_outputCCFdata[,],
       aes(x =subset_sum_join_outputCCFdata$anCCF_LAG, y = subset_sum_join_outputCCFdata$anCCF_ACF, group = subset_sum_join_outputCCFdata$an_CCF_ObjectID))+
  geom_rect(data=subset_sum_join_outputCCFdata, aes(ymin=-1, ymax=1, xmin=-15,
                                                    xmax=15, fill=subset_sum_join_outputCCFdata$meanLAGzero, color = subset_sum_join_outputCCFdata$meanLAGzero), alpha =0.1)+
  geom_line(alpha = 0.5)+
  scale_color_viridis_c(option ="magma")+
  scale_fill_viridis_c(option ="magma")+
  facet_grid( vars(an_CCF_Feature),vars(Treatment) )+
  stat_summary(aes(y = subset_sum_join_outputCCFdata$anCCF_ACF,group=1), fun.y=mean, colour="darkorange", geom="line",group=1, size = 1)+
  theme_classic() +
  theme(legend.position="bottom")

#create dendrogram plot
draw_treatmentDendrogram <- ggdendrogram(treatmentDendrogram) #dendrogram option 1

#create second dendrogram option
dend_data <- dendro_data(treatmentDendrogram)
# Setup the data, so that the layout is inverted (this is more
# "clear" than simply using coord_flip())
segment_data <- with(
  segment(dend_data),
  data.frame(x = y, y = x, xend = yend, yend = xend))
# Use the dendrogram label data to position the gene labels
treat_pos_table <- with(
  dend_data$labels,
  data.frame(y_center = x, treat = as.character(label), height = 1))
#create a dend plot
plt_dendr <- ggplot(segment_data) +  #dendrogram option 2
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
  coord_flip()+
  # scale_x_reverse(expand = c(0, 0.5)) +
  scale_y_continuous(breaks = treat_pos_table$y_center,
                     labels = treat_pos_table$treat,
                     # limits = treat_axis_limits,
                     expand = c(0, 0.5)) +
  labs(x = "Distance", y = "", colour = "", size = "") +
  theme_bw() +
  theme(panel.grid.minor = element_blank())


#http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/
# combine plots

library("grid")
# grid.newpage()
# print(heatmap_CCF, vp = viewport(x = 0.4, y = 0.5, width = 0.8, height = 1))
# print(draw_treatmentDendrogram, vp = viewport(x = 0.4, y = 0.9, width = 0.8, height = 1))

library(ggpubr)
ggarrange(plt_dendr, heatmap_CCF + rremove("x.text"),
          heights = c(1, 4),
          align = "v",
          # labels = c("A", "B", "C"),
          ncol = 1, nrow = 2)
#
#
#
# grid.newpage()
# print(heatmap_CCF, vp=viewport(0.8, 0.8, x=0.4, y=0.4))
# print(draw_treatmentDendrogram, vp=viewport(0.52, 0.2, x=0.45, y=0.9))
# # print(p3, vp=viewport(0.2, 0.8, x=0.9, y=0.4))
#
# library(grid)
# library(gridExtra)
# grid.arrange(draw_treatmentDendrogram, heatmap_CCF, nrow = 2)
# ?grid()
# ?viewport()

#plot aggregated data and no clustering
library(ggplot2)
ggplot(subset_sum_join_outputCCFdata, aes(x =anCCF_LAG, y = anCCF_ACF, group = an_CCF_ObjectID))+
  geom_line(alpha = 0.1)+
  facet_wrap(~an_CCF_Feature)+
  stat_summary(aes(y = anCCF_ACF,group=1), fun.y=mean, colour="darkorange", geom="line",group=1, size = 1)+
  theme_classic()

# # join_outputCCFdata
# ggplot(subset_sum_join_outputCCFdata, aes(x =anCCF_LAG, y = anCCF_ACF, group = an_CCF_ObjectID, color = Treatment))+
#   geom_line(alpha = 0.1)+
#   facet_grid( vars(an_CCF_Feature),vars(Treatment) )+
#   stat_summary(aes(y = anCCF_ACF,group=1), fun.y=mean, colour="gray20", geom="line",group=1, size = 1)+
#   theme_classic()

#Clustered, and coloured by mean, with identicle feature removed
ggplot(subset_sum_join_outputCCFdata[,],
       aes(x =subset_sum_join_outputCCFdata$anCCF_LAG, y = subset_sum_join_outputCCFdata$anCCF_ACF, group = subset_sum_join_outputCCFdata$an_CCF_ObjectID, color = subset_sum_join_outputCCFdata$meanLAGzero))+
  geom_line(alpha = 0.5)+
  scale_color_viridis_c(option ="magma")+
  facet_grid( vars(an_CCF_Feature),vars(Treatment) )+
  stat_summary(aes(y = subset_sum_join_outputCCFdata$anCCF_ACF,group=1), fun.y=mean, colour="gray20", geom="line",group=1, size = 1)+
  theme_classic()

# #Clustered and background coloured
# ggplot(subset_sum_join_outputCCFdata[,],
#        aes(x =subset_sum_join_outputCCFdata$anCCF_LAG, y = subset_sum_join_outputCCFdata$anCCF_ACF, group = subset_sum_join_outputCCFdata$an_CCF_ObjectID))+
#   geom_rect(data=subset_sum_join_outputCCFdata, aes(ymin=-1, ymax=1, xmin=-15,
#                                                     xmax=15, fill=subset_sum_join_outputCCFdata$meanLAGzero, color = subset_sum_join_outputCCFdata$meanLAGzero), alpha =0.1)+
#
#   geom_line(alpha = 0.5)+
#   scale_color_viridis_c(option ="magma")+
#   scale_fill_viridis_c(option ="magma")+
#   facet_grid( vars(an_CCF_Feature),vars(Treatment) )+
#   stat_summary(aes(y = subset_sum_join_outputCCFdata$anCCF_ACF,group=1), fun.y=mean, colour="darkorange", geom="line",group=1, size = 1)+
#   theme_classic()

