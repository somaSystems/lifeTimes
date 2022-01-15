#' lts_couplingPlot
#'
#' @import ggplot2
#' @import magrittr
#' @importFrom magrittr %>%
#' @importFrom viridis scale_color_viridis
#'
#' @param .lts_output results of lts_input() function, includes
#'cross correlation calculations and user input variables
#'
#' @export
#'
#'
#'
#'
lts_couplingPlot <- function(.lts_output = NULL){

# lts_couplingPlot <- function(.lts_clusterOutput_LAGranges = lts_clusterOutput_LAGranges, .lts_variables = NULL){
# # .lts_variables = lts_variables
# # .lts_clusterOutput_LAGranges = lts_clusterOutput_LAGranges


  # .lts_output$lts_rawCCFout$modeMaxCorrLAG

  if(is.null(.lts_output)){
    return(print("please enter some lifeTimes output"))
    # .lts_variables <- lts_defaultVariables
    # .lts_clusterOutput_LAGranges <- lts_OUT_lts_clusterOutput_LAGranges
  }
.lts_final_clusters <- .lts_output$lts_CCFcalcs

category1 <-  .lts_output$lts_variables$lts_compare_by[[1]]
category2 <-  .lts_output$lts_variables$lts_compare_by[[2]]
.lts_final_clusters[.lts_final_clusters$theLAG ==  .lts_output$lts_rawCCFout$modeMaxCorrLAG,]

berryTwig <- ggplot(data = .lts_final_clusters[.lts_final_clusters$theLAG == 1,])+
  annotate("rect",xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,  fill = "#2c7da0",alpha = 0.3)+
  annotate("rect",xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf,  fill = "#fff3b0",alpha = 0.3)+
  geom_point(aes(x = medianPrePostPerTF, y = meanCorrAtModeMaxLAG, color = !!sym(category1), size = 2))+ #need to make this a variable
  geom_segment(aes(xend = medianPrePostPerTF, yend = meanCorrAtModeMaxLAG,  x=0,y=0),alpha = 0.01)+
  viridis::scale_color_viridis(option = "magma", discrete =TRUE)+
  facet_wrap(vars(!!sym(category2)))+ #need to make this a variable
  theme_classic()+
  geom_vline(xintercept = 0, alpha = 0.2)

 return(berryTwig)
}








