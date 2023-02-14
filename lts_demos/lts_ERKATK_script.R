
if(!require(gsignal)){install.packages("gsignal")}
library(gsignal)

# T <- 10 * (1 / 50)
# fs <- 1000
# t <- seq(0, T-1/fs, 1/fs)
# y <- sawtooth(2 * pi * 50 * t, 1/2)
# plot(t, y, type="l", xlab = "", ylab = "", main = "50 Hz triangle wave")
#
# plot(t, y, type="l", xlab = "", ylab = "", main = "50 Hz triangle wave")
#
# # plot(sin(0.01*(x+0.4)))
#
# #sample data generation
#
# # y = abs((x++ % 6) - 3)
#
# # https://stat.ethz.ch/pipermail/r-help/2012-March/308173.html
#
# x<- seq(0, 10, length = 1000)
# y<- x %%1
#
#
# x<- runif(500, min = -2, max = 2)
# x
# y<- (1 -abs(x*3))* ((x*3)<= 1)
# y
# combined<- data.frame(x = x*3, y = y*3)
# plot(combined)
#
# plot(x,y)



head(lts_ERKAKT_max)
library(lifeTimes)

lts_test <- lts_in(.in_clusterBy = "portions")


lts_test$lts_ccf_summaries$lts_catGroups_portions

lts_plot_ccfs(lts_test)

lts_test$lts_ccf_summaries$lts_catGroups_portions

# lts_test$

lts_plot_ccfs(lts_test)


str(lts_test$lts_ccf_summaries$lts_catGroups_portions$cat2_portion)

lts_test$lts_ccf_summaries$lts_catGroups_summ_modeMaxCorrLAG

lts_test$lts_ccf_summaries$lts_catGroups_portions

lts_test$lts_ccf_summaries$lts_catGroups_portions$cat2_portion


erkakt


####Test portions
lts_ERKAKT_max <- readRDS(file="../lifetimes_testWorkflows/lts_ERKAKT_max_clustered.rds")
erkakt <- lts_ERKAKT_max$lts_variables$lts_data





library(lifeTimes)
lts_pairs <- lts_pairsMaker(c("ERK","AKT"))

print(lts_pairs)

lts_learn <- lts_in()
lts_learn$lts_variables$lts_pariedComparisons

lts_ERKAKT_max <- lts_in(erkakt,
                         .in_time = "timepoint",
                         .in_compare_categorical = c("class_name","sub_grp"),
                         .in_plot_measured_variables = FALSE,
                         .in_pairedComparisons = lts_pairs,
                         .in_uniqueID_colname = "ID",
                         .in_lagMax = 199,
                         .in_clusterBy = "portions" )

# getwd()
saveRDS(lts_ERKAKT_max, file = "../lifetimes_testWorkflows/ERKAKT_199Lag.rds")

lts_ERKAKT_max$lts_clust_outputs
lts_ERKAKT_max$lts_clust_outputs$clust_column_feature1
lts_ERKAKT_max$lts_clust_outputs$clust_row_feature2


lts_plot_ccfs(lts_ERKAKT_max)
# lts_catGroups_summ_modeMaxCorrLAG <- .lts_ccfWithMetaData %>%
#   # dplyr::select(theLAG, theCCF, !!rlang::sym(.lts_uniqueID_colname))%>% #group by unique ID
#   dplyr::group_by(!!rlang::sym(.lts_uniqueID_colname),
#                   theFeature #hotfix March 2022 to make work for single categoricals
#   )%>% #group by unique IDS
#   dplyr::slice_max(theCCF, n =1, with_ties = FALSE) %>% #get the top CCF for each ID
#   dplyr::ungroup()%>% #ungroup to so just left with IDs and their max
#   dplyr::rename(maxCCF = theCCF, LAGatMaxCCF =theLAG)%>% # rename the columns to reflect that they are Max values
#   dplyr::group_by(!!rlang::sym(.lts_compare_by[[1]]), #group by categorical variables
#                   !!rlang::sym(.lts_compare_by[[2]])) %>%
#   # dplyr::ungroup()%>% #ungroup to just get the Mode LAG of everything
#   dplyr::summarise(catGroups_ModeLAGatMax = Mode(LAGatMaxCCF), #get mode LAG with Max
#                    catGroups_mean_corr_atModeLAG = mean(maxCCF)) #get mean corr at mode Lag

lts_catGroups_summ_modeMaxCorrLAG <- .lts_ccfWithMetaData %>%
  # dplyr::select(theLAG, theCCF, !!rlang::sym(.lts_uniqueID_colname))%>% #group by unique ID
  dplyr::group_by(!!rlang::sym(.lts_uniqueID_colname),
                  theFeature #hotfix March 2022 to make work for single categoricals
  )%>% #group by unique IDS
  dplyr::slice_max(theCCF, n =1, with_ties = FALSE) %>% #get the top CCF for each ID
  dplyr::ungroup()%>% #ungroup to so just left with IDs and their max
  dplyr::rename(maxCCF = theCCF, LAGatMaxCCF =theLAG)%>% # rename the columns to reflect that they are Max values
  dplyr::group_by(!!rlang::sym(.lts_compare_by[[1]]), #group by categorical variables
                  !!rlang::sym(.lts_compare_by[[2]])) %>%
  # dplyr::ungroup()%>% #ungroup to just get the Mode LAG of everything
  dplyr::summarise(catGroups_ModeLAGatMax = Mode(LAGatMaxCCF), #get mode LAG with Max
                   catGroups_mean_corr_atModeLAG = mean(maxCCF)) #get mean corr at mode Lag




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



.lts_uniqueID_colname <- lts_ERKAKT_max$lts_variables$lts_uniqueID_colname
.lts_uniqueID_colname

library(dplyr)

# lts_ERKAKT_max$lts_ccf_summaries$lts_catGroups_portions %>%
#   dplyr::group_by(!!rlang::sym(.lts_uniqueID_colname),
#                   theFeature #hotfix March 2022 to make work for single categoricals
#                   )

#test portions

lts_plot_ccfs(lts_ERKAKT_max)
lts_plot_ClustSum(lts_ERKAKT_max)
lts_plot_coupled(lts_ERKAKT_max,.lts_facet_by = "cat1",.lts_colour_by = "cat2" )
?lts_plot_coupled


portion_tester <- (lts_ERKAKT_max$lts_ccfs_with_meta$lts_metadf)
head(portion_tester)
.lts_ccfWithMetaData <- portion_tester
.lts_uniqueID_colname <- lts_ERKAKT_max$lts_variables$lts_uniqueID_colname
.lts_compare_by <- lts_ERKAKT_max$lts_variables$lts_compare_by
.lts_compare_by


#define mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#use this one
lts_compare_by_1_totals <- .lts_ccfWithMetaData %>%
  # dplyr::select(theLAG, theCCF, !!rlang::sym(.lts_uniqueID_colname))%>% #group by unique ID
  dplyr::group_by(!!rlang::sym(.lts_compare_by[[1]])) %>% #group by categorical variables
  dplyr::tally(name = "n_cat1")

lts_compare_by_1_with_compare_by_2_breakdown <- .lts_ccfWithMetaData %>%
  dplyr::group_by(!!rlang::sym(.lts_compare_by[[1]]), #group by categorical variables
                  !!rlang::sym(.lts_compare_by[[2]])) %>%
  dplyr::tally(name = "n_cat2_per_cat1")


lts_compare_by_portions <- dplyr::left_join(lts_compare_by_1_totals, lts_compare_by_1_with_compare_by_2_breakdown, by = "class_name")

lts_compare_by_portions$cat2_portion <- lts_compare_by_portions$n_cat2_per_cat1/lts_compare_by_portions$n_cat1


lts_compare_by_portions

# lts_compare_by_portions %>%
#   dplyr::group_by(!!rlang::sym(.lts_compare_by[[1]]),
#                   !!rlang::sym(.lts_compare_by[[2]])) %>%
#   dplyr::summarise()

sum(lts_compare_by_portions$cat2_portion)



lts_ERKAKT_max$lts_ccf_summaries$lts_catGroups_mut_modeMaxCorrLAG

##Insert the code
##Allow for colour and facet by this
##Enter as argument at variables cluster_by_portion = TRUE

#check how clustering currently works


  dplyr::group_by(!!rlang::sym(.lts_uniqueID_colname),
                  theFeature #hotfix March 2022 to make work for single categoricals
  )%>% #group by unique IDS
  dplyr::slice_max(theCCF, n =1, with_ties = FALSE) %>% #get the top CCF for each ID
  dplyr::ungroup()%>% #ungroup to so just left with IDs and their max
  dplyr::rename(maxCCF = theCCF, LAGatMaxCCF =theLAG)%>% # rename the columns to reflect that they are Max values
  dplyr::group_by(!!rlang::sym(.lts_compare_by[[1]]), #group by categorical variables
                  !!rlang::sym(.lts_compare_by[[2]])) %>%
  # dplyr::ungroup()%>% #ungroup to just get the Mode LAG of everything
  dplyr::summarise(catGroups_ModeLAGatMax = Mode(LAGatMaxCCF), #get mode LAG with Max
                   catGroups_mean_corr_atModeLAG = mean(maxCCF)) #get mean corr at


lts_catGroups_summ_facet_portion <- .lts_ccfWithMetaData %>%
  # dplyr::select(theLAG, theCCF, !!rlang::sym(.lts_uniqueID_colname))%>% #group by unique ID
  dplyr::group_by(!!rlang::sym(.lts_uniqueID_colname),
                  theFeature #hotfix March 2022 to make work for single categoricals
  )%>% #group by unique IDS
  dplyr::slice_max(theCCF, n =1, with_ties = FALSE) %>% #get the top CCF for each ID
  dplyr::ungroup()%>% #ungroup to so just left with IDs and their max
  dplyr::rename(maxCCF = theCCF, LAGatMaxCCF =theLAG)%>% # rename the columns to reflect that they are Max values
  dplyr::group_by(!!rlang::sym(.lts_compare_by[[1]]), #group by categorical variables
                  !!rlang::sym(.lts_compare_by[[2]])) %>%
  # dplyr::ungroup()%>% #ungroup to just get the Mode LAG of everything
  dplyr::summarise(catGroups_ModeLAGatMax = Mode(LAGatMaxCCF), #get mode LAG with Max
                   catGroups_mean_corr_atModeLAG = mean(maxCCF)) #get mean corr at


lts_catGroups_summ_facet_portion



#get average i.e get CCF motif
# show closest x number to motifs.



lts_ERKAKT_max$lts_ccfs_with_meta

library(dplyr)

colnames(as.data.frame(lts_ERKAKT_max$lts_ccfs_with_meta))

lts_motifs <- as.data.frame(lts_ERKAKT_max$lts_ccfs_with_meta) %>%
  dplyr::group_by(lts_metadf.sub_grp, lts_metadf.theLAG)%>%
  dplyr::mutate(motif_pos = mean(lts_metadf.theCCF))

lts_motifs_unq <- lts_motifs %>%
  dplyr::select(lts_metadf.theLAG, lts_metadf.sub_grp, motif_pos)%>%
  unique()

dim(lts_motifs)
dim(lts_motifs_unq)
ggplot(lts_motifs_unq, aes(x = lts_metadf.theLAG, y = motif_pos))+
  geom_line(size = 1)+
  facet_wrap(~lts_metadf.sub_grp, nrow = 1)+
  theme_classic()

#to do need to remove to unqiue

nDiffs <- 1

#find squared difference between each CCF and the
lts_unwind <- lts_motifs %>%
  dplyr::mutate(square_diff = (lts_metadf.theCCF - motif_pos)^2)%>% # calculate square difference
  dplyr::group_by(lts_metadf.sub_grp) %>% #grouping in by cluster(individual time traces, per cluster)
  dplyr::mutate(sum_square_diffs = sum(square_diff)) %>%
  dplyr::slice_min(square_diff, n = nDiffs) %>%
  dplyr::select(lts_metadf.ID, lts_metadf.sub_grp)%>%
  unique()


lts_unwind
lts_examplarIDs <- c(lts_unwind$lts_metadf.ID)
lts_examplarIDs

lts_originalERKAKT <- lts_ERKAKT_max$lts_variables$lts_data

library(tidyr)

lts_motif_examples <- lts_originalERKAKT %>%
  dplyr::filter(ID %in% lts_examplarIDs) %>%
  pivot_longer(cols = c(ERK, AKT),
               names_to = "lts_motif_measures",
               values_to = "lts_motif_values")

lts_motif_examples$class_name

length(unique(lts_motif_examples$ID))


ggplot(lts_motif_examples, aes(x = as.numeric(timepoint), y = lts_motif_values,  color = lts_motif_measures, group = lts_motif_measures))+
  geom_line(size = 1)+
  scale_color_manual(values = c("darkorange","dodgerblue"))+
  facet_wrap(~sub_grp, nrow = 1)+
  theme_classic()+
  coord_cartesian(xlim = c(0,200))+
  theme(legend.position="bottom")


lts_in()
lts_originalERKAKT$ID_class <- lts_originalERKAKT$ID
lts_originalERKAKT$sub_grp
examples<- lts_originalERKAKT%>% dplyr::filter(ID %in% lts_examplarIDs)%>%
  droplevels()

# examples$class_name

lts_pairs

lts_motifs_ccf <- lts_in(examples,
       .in_time = "timepoint",
       .in_compare_categorical = c("sub_grp"),
       .in_plot_measured_variables = TRUE,
       .in_pairedComparisons = lts_pairs,
       .in_uniqueID_colname = "ID",
       .in_lagMax = 199)

lts_plot_ccfs(lts_motifs_ccf)

df_lts_motifs <- lts_motifs_ccf$lts_ccfs_with_meta$lts_metadf
ggplot(df_lts_motifs, aes(x = as.numeric(theLAG), y = theCCF))+
  geom_line(size = 1, color = "orchid4")+
  # scale_color_manual(values = c("coral"))+
  facet_wrap(~ sub_grp, nrow = 1)+
  theme_classic()


# longOriginal, pivot_longer(lts_originalERKAKT,
#
#
#
# ggplot(lts_ERKAKT_max$lts_variables$lts_data, aes( x= ))


dim(lts_motifs)
dim(lts_unwind)

?top_frac
lts_unwind

#Visualise motifs
library(ggplot2)
# ggplot(lts_motifs, aes(x = lts_metadf.theLAG, y = motif_pos, group = lts_metadf.ID))+
#   geom_line(size = 1)+
#   facet_wrap(~lts_metadf.sub_grp)+
#   theme_classic()


