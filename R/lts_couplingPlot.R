#' lts_couplingPlot
#'
#' @import ggplot2
#' @import magrittr
#' @importFrom magrittr %>%
#' @importFrom viridis scale_color_viridis
#'
#' @param .lts_clusterOutput_LAGranges list with dataframe of
#' CCFs returned from lifeTimesChain()
#' @param .lts_variables user defined variables mapping data to function arguments
#'
#' @export
#'
#' @example lts_couplingPlot(outPutCCF <- lifeTimesChain() )
#'
#'
#'

lts_couplingPlot <- function(.lts_clusterOutput_LAGranges = lts_clusterOutput_LAGranges, .lts_variables = lts_variables){
# .lts_variables = lts_variables
# .lts_clusterOutput_LAGranges = lts_clusterOutput_LAGranges
.lts_final_clusters <- lts_clusterOutput_LAGranges$medDiff_meanLag_lts_clusterCCFs

category1 <-  lts_variables$lts_compare_by[[1]]
category2 <-  lts_variables$lts_compare_by[[2]]
.lts_final_clusters[.lts_final_clusters$theLAG == .lts_clusterOutput_LAGranges$modeMaxCorrLAG,]

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








