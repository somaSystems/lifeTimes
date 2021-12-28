#' couplingPlot !
#'
#' @import ggplot2
#' @importFrom viridis scale_color_viridis
#'
#' @param join_medianDiff_meanLagRange_outputCCFdata_withMetaData dataframe of
#' CCFs returned from lifeTimesChain()
#'
#'
#' @export
#'
#' @examples couplingPlot(lifeTimesChain())
couplingPlot <- function(join_medianDiff_meanLagRange_outputCCFdata_withMetaData){


berryTwig <- ggplot(data = join_medianDiff_meanLagRange_outputCCFdata_withMetaData[join_medianDiff_meanLagRange_outputCCFdata_withMetaData$lagRange =="zeroLAG",])+
  annotate("rect",xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,  fill = "#2c7da0",alpha = 0.1)+
  annotate("rect",xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf,  fill = "#fff3b0",alpha = 0.1)+
  geom_point(aes(x = medianPrePostPerTF, y = meanLAGzero, color = an_CCF_Feature), size = 2)+
  geom_segment(aes(xend = medianPrePostPerTF, yend = meanLAGzero,  x=0,y=0),alpha = 0.01)+
  viridis::scale_color_viridis(option = "magma", discrete =TRUE)+
  facet_wrap(~Treatment)+
  theme_classic()+
  geom_vline(xintercept = 0, alpha = 0.2)

berryTwig
return(eval(berryTwig))
}
