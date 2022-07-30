
lts_ERKAKT_max <- readRDS(file="../lifetimes_testWorkflows/lts_ERKAKT_max_clustered.rds")
head(lts_ERKAKT_max)
library(lifeTimes)

lts_test <- lts_in()

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
