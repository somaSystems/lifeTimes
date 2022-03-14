#' lts_clusterPlot
#' Returns plot of ccfs and dendrogram (compound plot), facets are clustered by the mean correlation at the lag with the most frequent (mode) maximum correlation. Future updates will allow decomposition of plots into component parts.
#' @import ggplot2
#' @import gridExtra
#' @import grid
#' @import egg
#' @importFrom ggplot2 ggplot
#' @importFrom ggpubr rremove
#' @importFrom ggdendro ggdendrogram dendro_data segment
#'
#' @param .lts_output output from the lifeTimes main workflow
#'
#'
#' @param plotType choice from c("compoundPlot","draw_treatmentDendrogram",
#' "plt_dendr","heatmapLagZero","rawTraces","clusteredLines")
#'
#' @return a plot of features and treatments clustered by CCF at lag zero
#'
#' @export
#'

# lts_clusterOutput
lts_plot_ccfs <- function(
  .lts_output = lts_cluster,
   plotType = c("compoundPlot","draw_treatmentDendrogram","plt_dendr","heatmapLagZero","rawTraces","clusteredLines")){


# lts_plot_ccfs <- function(
  # .lts_output = lts_garmin
  # plotType = c("compoundPlot")
  # # {


# # lts_clusterPlot <- function(
#   .lts_output = lts_cluster
#   plotType = c("compoundPlot","draw_treatmentDendrogram","plt_dendr","heatmapLagZero","rawTraces","clusteredLines"))

  if(is.null(.lts_output)){
    print("enter results from lts_calc()")
    # .lts_variables <- lts_defaultVariables
    # .lts_clusterOutput_LAGranges <- lts_OUT_lts_clusterOutput_LAGranges
  }


  # subset_sum_join_outputCCFdata <-  .lts_output$lts_CCFcalcs
  #
  # subset_sum_join_outputCCFdata <- .lts_output$lts_clust_ccfs_with_metadata$lts_clust_ccfs_with_meta



.lts_output$lts_rawCCFout$lts_mCCF_chosenLAG #this is the matrix of correlations at the chosen type of lag



  subset_sum_join_outputCCFdata <- .lts_output$lts_clust_ccfs_with_meta
  .lts_theLAG <- .lts_output$lts_clust_ccfs_with_meta$theLAG
  clust_matrix <- .lts_output$lts_clust_outputs$clust_matrix
  .summ_for_matrix <- .lts_output$lts_ccf_summaries$lts_catGroups_summ_modeMaxCorrLAG

  plotType <- match.arg(plotType)
  ensym_plotType <- rlang::sym(plotType)

  treatmentDendrogram <- as.dendrogram(hclust(dist(t(clust_matrix)))) #make dendrogram
  draw_treatmentDendrogram <- ggdendro::ggdendrogram(treatmentDendrogram)
  draw_treatmentDendrogram
  dend_data <- ggdendro::dendro_data(treatmentDendrogram)

  # Setup the data, so that the layout is inverted (this is more
  # "clear" than simply using coord_flip())
  segment_data <- with(
    ggdendro::segment(dend_data),
    data.frame(x = y, y = x, xend = yend, yend = xend))

  # Use the dendrogram label data to position the gene labels
  treat_pos_table <- with(
    dend_data$labels,
    data.frame(y_center = x, treat = as.character(label), height = 1))

  #create a dend plot
  plt_dendr <- ggplot2::ggplot(segment_data) +  #dendrogram option 2
    ggplot2::geom_segment(ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
    ggplot2::coord_flip()+
    # scale_x_reverse(expand = c(0, 0.5)) +
    ggplot2::scale_y_continuous(breaks = treat_pos_table$y_center,
                                labels = treat_pos_table$treat,
                                # limits = treat_axis_limits,
                                expand = c(0, 0.5)) +
    ggplot2::labs(x = "Distance", y = "", colour = "", size = "") +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())


  #make dataframe for annotation rectangles in each facet
  #needs a row for each facet, or each combination of factor 1 and factor 2)
  #fix for features
  ##NB: if conditional works, can just make it always the case that facetsRequired, and category names and contents are taken from the CCFcalcs list



  if(.lts_output$lts_variables$lts_plot_measured_variables == TRUE){ #include "theFeature" in lts_compare_by, for plotting
    facetsRequired <-
      length(levels( subset_sum_join_outputCCFdata[,.lts_output$lts_variables$lts_compare_by[[1]]]))*
      length(levels( subset_sum_join_outputCCFdata[,.lts_output$lts_variables$lts_compare_by[[2]]])) #could not get levels() and length() to work here so have used nrow() and unique())

    category1_name <- .lts_output$lts_variables$lts_compare_by[[1]]
    category1_contents <- unique( subset_sum_join_outputCCFdata[,.lts_output$lts_variables$lts_compare_by[[1]]])
    category1_levels <- levels( subset_sum_join_outputCCFdata[,category1_name])

    category2_name <- .lts_output$lts_variables$lts_compare_by[[2]]
    category2_contents <- unique( subset_sum_join_outputCCFdata[,.lts_output$lts_variables$lts_compare_by[[2]]])
    category2_levels <- levels( subset_sum_join_outputCCFdata[,category2_name])

  }else ##Added this too

  {
    facetsRequired <-
      length(levels(.lts_output$lts_variables$lts_data[,.lts_output$lts_variables$lts_compare_by[[1]]]))*
      length(levels(.lts_output$lts_variables$lts_data[,.lts_output$lts_variables$lts_compare_by[[2]]]))

    category1_name <- .lts_output$gridExtralts_variables$lts_compare_by[[1]]
    category1_contents <- unique(.lts_output$lts_variables$lts_data[,.lts_output$lts_variables$lts_compare_by[[1]]])
    category1_levels <- levels( subset_sum_join_outputCCFdata[,category1_name])

    category2_name <- .lts_output$lts_variables$lts_compare_by[[2]]
    category2_contents <- unique(.lts_output$lts_variables$lts_data[,.lts_output$lts_variables$lts_compare_by[[2]]])
    category2_levels <- levels( subset_sum_join_outputCCFdata[,category2_name])
  }



  #create unique facet labels
  #TODO: check that this is robust to switching categories lengths
  heatmapAnno <- data.frame(
    df_category1_name = paste(rep(category1_contents,
                                  times = facetsRequired/length(category1_contents), #number of times for length of cat to fill all facets
                                  each = 1)),  #loop through all

    df_category2_name = paste(rep(category2_contents,
                                  # times = facetsRequired/length(category1_contents), # eg, print these once for each set of cat 1, i.e same no of times
                                  each = (facetsRequired/length(category2_contents)))), # non alternating version of multiples of "times" needed to reach requiredFaces

    xmin =  paste(rep(min(.lts_theLAG), time = length(facetsRequired))),
    xmax =  paste(rep(max(.lts_theLAG),  time = length(facetsRequired))),
    ymin = paste(rep(-Inf, time = length(facetsRequired))),
    ymax = paste(rep(Inf, time = length(facetsRequired)))
  )


  #correct column names
  names(heatmapAnno)[names(heatmapAnno) == 'df_category1_name'] <-category1_name
  names(heatmapAnno)[names(heatmapAnno) == 'df_category2_name'] <-category2_name

  #here colour rectangle values by clustering matrix values
  #go to source of cluster matrix gridExtra(a summary sheet)

  # .summ_for_matrix
  # lts2$lts_CCFcalcs
  rectValues <- dplyr::left_join(heatmapAnno,  #join unique react values to column names
                                 # .lts_output$lts_CCFcalcs[c("meanCorrAtModeMaxLAG", category1_name, category2_name)],
                                 .summ_for_matrix[c("catGroups_mean_corr_atModeLAG", category1_name, category2_name)],
                                 by = c(category1_name, category2_name))

  # unq_rectValues <- unique(rectValues)

  lts_heatmapAnno <- dplyr::left_join(heatmapAnno, rectValues[c("catGroups_mean_corr_atModeLAG", category1_name, category2_name)], by = c(category1_name, category2_name))
  # lts_heatmapAnno


  lts_heatmapAnno$xmin <- as.numeric(lts_heatmapAnno$xmin)
  lts_heatmapAnno$xmax<- as.numeric(lts_heatmapAnno$xmax)
  lts_heatmapAnno$ymin<- as.numeric(lts_heatmapAnno$ymin)
  lts_heatmapAnno$ymax<- as.numeric(lts_heatmapAnno$ymax)
  lts_heatmapAnno[category1_name] <-  factor(lts_heatmapAnno[,.lts_output$lts_variables$lts_compare_by[[1]] ], levels = category1_levels)
  lts_heatmapAnno[category2_name] <-  factor(lts_heatmapAnno[,.lts_output$lts_variables$lts_compare_by[[2]] ], levels = category2_levels)
  # str(lts_heatmapAnno)

  heatmapLagZero <- ggplot2::ggplot()+
    ggplot2::geom_rect(data=lts_heatmapAnno,
              ggplot2::aes(ymin=-Inf, ymax=Inf,
                  xmin=-Inf, xmax=Inf,
                  fill= catGroups_mean_corr_atModeLAG), alpha =0.5)+
    ggplot2::geom_line(data = subset_sum_join_outputCCFdata, ggplot2::aes(x =theLAG,
                                                        y = theCCF,
                                                        group = !!sym(.lts_output$lts_variables$lts_uniqueID_colname)), alpha = 0.5, color = "black")+  ##fix this instance of keynum
    ggplot2::scale_color_viridis_c("mean Corr at most\n  frequent max lag", option ="magma")+
    ggplot2::scale_fill_viridis_c("mean Corr at most\n  frequent max lag", option ="magma")+
    ggplot2::facet_grid( vars(!!sym(category2_name)), vars(!!sym(category1_name)) )+
    ggplot2::stat_summary(data = sgridExtraubset_sum_join_outputCCFdata,ggplot2::aes(x = theLAG, y = theCCF,group=1), fun=mean, colour="darkorange", geom="line",group=1, size = 1)+
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position="bottom")+
    # + ylab() +
    ggplot2::theme(strip.text.y.right = ggplot2::element_text(angle = 0)) #remove this if a problem
    # ggplot2::guides(fill=  ggplot2:::guide_legend(title="mean Corr at most\n  frequent max lag"))

# info on guides labels
# https://stackoverflow.com/questions/14622421/how-to-change-legend-title-in-ggplot
  # heatmapLagZero

  # compoundPlot <- ggpubr::ggarrange(plt_dendr, heatmapLagZero + ggpubr::rremove("x.text"),
  #                                   heights = c(2, 7),
  #                                   align = "v",
  #                                   # labels = c("A", "B", "C"),
  #                                   ncol = 1, nrow = 2)



##egg and grid approach to compound plot for greater control of panel alignment

  p1 <- plt_dendr

  p2 <- heatmapLagZero
  g1 <- ggplot2::ggplotGrob(p1)
  g2 <- ggplot2::ggplotGrob(p2)

  fg1 <- egg::gtable_frame(g1, width = unit(1, "null"),
                           height = unit(.3, "null"),
                           debug = FALSE)

  fg2 <- egg::gtable_frame(g2, width = unit(1, "null"),
                           height = unit(1.7, "null"),
                           debug = FALSE)
  fg12 <-
    egg::gtable_frame(gridExtra::gtable_rbind(fg1, fg2),
                      width = unit(2, "null"),
                      height = unit(3, "null"))

  compoundPlot <- grid::grid.newpage()+grid::grid.draw(fg12)

  # compoundPlot

  # return((ensym_plotType))

    # return(eval(ensym_plotType))
 }


