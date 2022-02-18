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
#'
#' @export
#'


lts_couplePlot <- function(.lts_output = NULL,
                              .lts_facet_by = "cat1",
                              .lts_colour_by = "cat2",
                              .lts_chosen_clusterFeature = "posNegDiffmedian_corr_by_lag_range"){


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


.lts_final_clusters <- .lts_output$lts_ccf_summaries$lts_catGroups_summ_modeMax

subset_.lts_final_clusters <-
  unique(.lts_final_clusters[c(.lts_output$lts_variables$lts_compare_by,
                      "posNegDiffmedian_corr_by_lag_range",
                      "catGroups_mean_corr_atModeLAG",
                      "catGroups_ModeLAGatMax")])

# .lts_final_clusters[.lts_final_clusters$theLAG ==  ValueOf_modeMaxCorrLag,] #changed the lagTo meanCorratModeMa

berryTwig <- ggplot(data = subset_.lts_final_clusters)+ #changed the lagTo meanCorratModeMaxLag
  annotate("rect",xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,  fill = "#2c7da0",alpha = 0.3)+
  annotate("rect",xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf,  fill = "#fff3b0",alpha = 0.3)+
  geom_point(aes(x = posNegDiffmedian_corr_by_lag_range, y = catGroups_mean_corr_atModeLAG, color = !!rlang::sym(.lts_colour_by)))+ #need to make this a variable
  geom_segment(aes(xend = posNegDiffmedian_corr_by_lag_range, yend = catGroups_mean_corr_atModeLAG,  x=0,y=0),alpha = 0.05)+
  viridis::scale_color_viridis(option = "magma", discrete =TRUE)+
  facet_wrap(vars(!!rlang::sym(.lts_facet_by)))+ #need to make this a variable
  theme_classic()+
  geom_vline(xintercept = 0, alpha = 0.2)

 berryTwig
 return(berryTwig)
 }
