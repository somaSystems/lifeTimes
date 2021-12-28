#' plotChartFromClustered
#' @import ggplot2
#' @importFrom ggpubr ggarrange rremove
#' @importFrom ggdendro ggdendrogram ggdendro
#' @param clusteredZeroLag output from lifeTimesChain()
#' @param plotType choice from c("compoundPlot","draw_treatmentDendrogram",
#' "plt_dendr","heatmapLagZero","rawTraces","clusteredLines")
#'
#' @return a plot of features and treatments clustered by CCF at lag zero
#' @export
#'
#'

# plotChartFromClustered!
#return plot from function (requires print() apparently)
#https://stackoverflow.com/questions/11799317/custom-function-ggplot-and-return-values#
#https://stackoverflow.com/questio@s/9057006/getting-strings-recognized-as-variable-names-in-r

clusterPlot <- function(clusteredZeroLag = df_clusteredZeroLag_withMetada, plotType = c("compoundPlot","draw_treatmentDendrogram","plt_dendr","heatmapLagZero","rawTraces","clusteredLines")){
  #create heatmap ggplot
  #https://stackoverflow.com/questions/4683405/function-default-arguments-and-named-values
  subset_sum_join_outputCCFdata <-  df_clusteredZeroLag_withMetada

  plotType <- match.arg(plotType)
  ensym_plotType <- rlang::sym(plotType)

  # !!sym(plotType) := eval(parse(text = plotType))
# ?match.arg()

  #create dendrogram plot
  draw_treatmentDendrogram <- ggdendro::ggdendrogram(treatmentDendrogram) #dendrogram option 1

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

  heatmapLagZero <- ggplot(subset_sum_join_outputCCFdata[,],
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

  #http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/
  # combine plots

  # library("grid")
  # grid.newpage()
  # print(heatmap_CCF, vp = viewport(x = 0.4, y = 0.5, width = 0.8, height = 1))
  # print(draw_treatmentDendrogram, vp = viewport(x = 0.4, y = 0.9, width = 0.8, height = 1))

  # library(ggpubr)
  compoundPlot <- ggpubr::ggarrange(plt_dendr, heatmapLagZero + rremove("x.text"),
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

  rawTraces <- ggplot(subset_sum_join_outputCCFdata, aes(x =anCCF_LAG, y = anCCF_ACF, group = an_CCF_ObjectID))+
    geom_line(alpha = 0.1)+
    facet_wrap(~an_CCF_Feature)+
    stat_summary(aes(y = anCCF_ACF,group=1), fun.y=mean, colour="darkorange", geom="line",group=1, size = 1)+
    theme_classic()+
    theme(legend.position="bottom")

  # # join_outputCCFdata
  # ggplot(subset_sum_join_outputCCFdata, aes(x =anCCF_LAG, y = anCCF_ACF, group = an_CCF_ObjectID, color = Treatment))+
  #   geom_line(alpha = 0.1)+
  #   facet_grid( vars(an_CCF_Feature),vars(Treatment) )+
  #   stat_summary(aes(y = anCCF_ACF,group=1), fun.y=mean, colour="gray20", geom="line",group=1, size = 1)+
  #   theme_classic()

  #Clustered, and coloured by mean, with identicle feature removed
  clusteredLines <- ggplot(subset_sum_join_outputCCFdata[,],
                           aes(x =subset_sum_join_outputCCFdata$anCCF_LAG, y = subset_sum_join_outputCCFdata$anCCF_ACF, group = subset_sum_join_outputCCFdata$an_CCF_ObjectID, color = subset_sum_join_outputCCFdata$meanLAGzero))+
    geom_line(alpha = 0.5)+
    scale_color_viridis_c(option ="magma")+
    facet_grid( vars(an_CCF_Feature),vars(Treatment) )+
    stat_summary(aes(y = subset_sum_join_outputCCFdata$anCCF_ACF,group=1), fun.y=mean, colour="gray20", geom="line",group=1, size = 1)+
    theme_classic()+
    theme(legend.position="bottom")
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
# return(compoundPlot)
  # print(ensym_plotType)

   return(eval(ensym_plotType))
}
