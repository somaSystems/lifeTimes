#' lts_couplingPlot
#'
#' @import ggplot2
#' @import magrittr
#' @importFrom magrittr %>%
#' @importFrom viridis scale_color_viridis
#'
#' @param .lts_output results of lts_input() function, includes
#'cross correlation calculations and user input variables
#' @param .lts_facet_by choose from categorical variables, category1, or category2, arguments are withtout "".
#' @param .lts_colour_by choose from categorical variables, category1, or category2, arguments are without "".
#' @param .lts_chosen_clusterFeature a feature from CCF summary statistics (lts_ccf_summaries) that will be used for heatmap clustering
#'
#' @export
#'

#ToDO
#allow change of LAG, currently modeMax corr lag
#allow change of facet by


 lts_plot_coupling <- function(.lts_output = NULL,
                              .lts_facet_by = "cat1",
                              .lts_colour_by = "cat2",
                              .lts_chosen_clusterFeature = "posNegDiffmedian_corr_by_lag_range"){
#
# # lts_couplingPlot <- function(
# library(ggplot2)
#   .lts_output = lts_cluster
#                              .lts_facet_by = "cat1"
#                              .lts_colour_by = "cat2"
#                              .lts_chosen_clusterFeature = "posNegDiffmedian_corr_by_lag_range"


  if(is.null(.lts_output)){
    return(print("please enter some lifeTimes output"))
  }


                            if(.lts_facet_by == "cat1"){
                            .lts_facet_by <-  .lts_output$lts_variables$lts_compare_by[[1]]
                            }


                             if(.lts_facet_by == "cat2"){
                            .lts_facet_by <-  .lts_output$lts_variables$lts_compare_by[[2]]
                            }

                             if(.lts_colour_by == "cat1"){
                               .lts_colour_by <-  .lts_output$lts_variables$lts_compare_by[[1]]
                             }


                             if(.lts_colour_by == "cat2"){
                               .lts_colour_by <-  .lts_output$lts_variables$lts_compare_by[[2]]
                             }



# category2 <-  .lts_output$lts_variables$lts_compare_by[[2]]

lts_cluster$lts_ccf_summaries$lts_catGroups_summ


.lts_final_clusters <- .lts_output$lts_ccf_summaries$lts_catGroups_summ_modeMaxCorrLAG #summaries of ]
.lts_final_clusters_correlation <- .lts_output$lts_ccf_summaries$lts_catGroups_summ_modeMaxCorrLAG
.lts_chosen_clusterFeature <- .lts_output$lts_rawCCFout$modeMaxCorrLAG

# lts_cluster$lts_clust_outputs
.lts_chosen_clusterFeature <- .lts_output$lts_ccf_summaries$lts_catGroups_summ[,!!rlang::sym(.lts_chosen_clusterFeature)] #chosen cluster feature 2022


#subset LAG in final clusters
.lts_final_clusters$max
lts_cluster$lts_ccf_summaries$lts_catGroups_mut_modeMaxCorrLAG
.lts_final_clusters$

.lts_final_clusters[.lts_final_clusters$theLAG ==  ValueOf_modeMaxCorrLag,] #changed the lagTo meanCorratModeMa

berryTwig <- ggplot(data = .lts_final_clusters)+ #changed the lagTo meanCorratModeMaxLag
  annotate("rect",xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,  fill = "#2c7da0",alpha = 0.3)+
  annotate("rect",xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf,  fill = "#fff3b0",alpha = 0.3)+
  geom_point(aes(x = medianPrePostPerTF, y = !!rlang::sym(.lts_chosen_clusterFeature), color = !!sym(.lts_colour_by)))+ #need to make this a variable
  geom_segment(aes(xend = !!rlang::sym(.lts_chosen_clusterFeature), yend = !!rlang::sym(.lts_chosen_clusterFeature),  x=0,y=0),alpha = 0.01)+
  viridis::scale_color_viridis(option = "magma", discrete =TRUE)+
  facet_wrap(vars(!!rlang::sym(.lts_facet_by)))+ #need to make this a variable
  theme_classic()+
  geom_vline(xintercept = 0, alpha = 0.2)

berryTwig
 return(berryTwig)
 }
