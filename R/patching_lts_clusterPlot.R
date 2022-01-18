#' lts_clusterPlot
#'
#' @import ggplot2
#' @importFrom ggplot2 ggplot
#' @importFrom ggpubr ggarrange rremove
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
lts_clusterPlot <- function(
  .lts_output = NULL, plotType = c("compoundPlot","draw_treatmentDendrogram","plt_dendr","heatmapLagZero","rawTraces","clusteredLines"))
{


  #
  # (.lts_clusterOutput_LAGranges = lts_clusterOutput_LAGranges,
  #  .lts_variables = NULL,
  #  plotType = c("compoundPlot","draw_treatmentDendrogram","plt_dendr","heatmapLagZero","rawTraces","clusteredLines"))

  # if(is.null(.lts_clusterOutput_LAGranges)){.lts_clusterOutput_LAGranges <- lts_clusterOutput_LAGranges.rda", package = "lifeTimes")) #use this until internal data works
  if(is.null(.lts_output)){
    .lts_variables <- lts_defaultVariables
    .lts_clusterOutput_LAGranges <- lts_OUT_lts_clusterOutput_LAGranges
  }
  subset_sum_join_outputCCFdata <-  .lts_output$lts_CCFcalcs


  #
  # if(is.null(.lts_variables)){
  #   # print(paste("not_assigned:",lts_defaultVariables))
  #   .lts_variables <- lts_defaultVariables
  #   # print(paste("assigned:",.lts_variables))
  # }

  #   # lts_clusterOutput_LAGranges
  # if(missing(.lts_clusterOutput_LAGranges)){
  #   .lts_clusterOutput_LAGranges <- lts_OUT_lts_clusterOutput_LAGranges.rda
  # }

  # if(missing(.lts_variables)){
  # }




  plotType <- match.arg(plotType)
  ensym_plotType <- rlang::sym(plotType)

  # .lts_clusterOutput_LAGranges = lts_clusterOutput_LAGranges
  # .lts_variables = lts_variables
  # lts_clusterOutput_LAGranges
  # .lts_clusterOutput_LAGranges$medDiff_meanLag_lts_clusterCCFs


  #create dendrogram for clustering of categorical variable 1

  # lts_dendrogram <- function(.lts_clusterOutput = lts_clusterOutput){

  treatmentDendrogram <- as.dendrogram(hclust(dist(t(.lts_output$lts_rawCCFout$lts_mCCF_chosenLAG)))) #make dendrogram
  # plot(treatmentDendrogram)
  # str(treatmentDendrogram)
  # plot(treatmentDendrogram)
  draw_treatmentDendrogram <- ggdendro::ggdendrogram(treatmentDendrogram)
  draw_treatmentDendrogram
  dend_data <- ggdendro::dendro_data(treatmentDendrogram)

  # Setup the data, so that the layout is inverted (this is more
  # "clear" than simply using coord_flip())
  segment_data <- with(
    ggdendro::segment(dend_data),
    data.frame(x = y, y = x, xend = yend, yend = xend))



  # ggdendro::UseMethod
  # ggdendro::dendro_data


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

  # subset_sum_join_outputCCFdata$te


  subset_sum_join_outputCCFdata
  # annotate individual facets
  # https://r-graphics.org/recipe-annotate-facet




  #make dataframe for annotation rectangles in each facet
  #needs a row for each facet, or each combination of factor 1 and factor 2)

  #fix for features
  ##NB: if conditional works, can just make it always the case that facetsRequired, and category names and contents are taken from the CCFcalcs list
  if(.lts_variables$lts_plot_measured_variables == TRUE){ #include "theFeature" in lts_compare_by, for plotting
    facetsRequired <-
      length(levels(.lts_output$lts_CCFcalcs[,.lts_output$lts_variables$lts_compare_by[[1]]]))*
      length(levels(.lts_output$lts_CCFcalcs[,.lts_output$lts_variables$lts_compare_by[[2]]])) #could not get levels() and length() to work here so have used nrow() and unique())

    category1_name <- .lts_output$lts_variables$lts_compare_by[[1]]
    category1_contents <- unique(.lts_output$lts_CCFcalcs[,.lts_output$lts_variables$lts_compare_by[[1]]])
    category1_levels <- levels(.lts_output$lts_CCFcalcs[,category1_name])


    category2_name <- .lts_output$lts_variables$lts_compare_by[[2]]
    category2_contents <- unique(.lts_output$lts_CCFcalcs[,.lts_output$lts_variables$lts_compare_by[[2]]])
    category2_levels <- levels(.lts_output$lts_CCFcalcs[,category2_name])



  }else ##Added this to

  {

    facetsRequired <-
      length(levels(.lts_output$lts_variables$lts_data[,.lts_output$lts_variables$lts_compare_by[[1]]]))*
      length(levels(.lts_output$lts_variables$lts_data[,.lts_output$lts_variables$lts_compare_by[[2]]])) #could not get levels() and length() to work here so have used nrow() and unique())


    category1_name <- .lts_output$lts_variables$lts_compare_by[[1]]
    category1_contents <- unique(.lts_output$lts_variables$lts_data[,.lts_output$lts_variables$lts_compare_by[[1]]])
    category1_levels <- levels(.lts_output$lts_CCFcalcs[,category1_name])


    category2_name <- .lts_output$lts_variables$lts_compare_by[[2]]
    category2_contents <- unique(.lts_output$lts_variables$lts_data[,.lts_output$lts_variables$lts_compare_by[[2]]])
    category2_levels <- levels(.lts_output$lts_CCFcalcs[,category2_name])
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

    xmin =  paste(rep(min(.lts_output$lts_CCFcalcs$theLAG), time = length(facetsRequired))),
    xmax =  paste(rep(max(.lts_output$lts_CCFcalcs$theLAG),  time = length(facetsRequired))),
    ymin = paste(rep(-Inf, time = length(facetsRequired))),
    ymax = paste(rep(Inf, time = length(facetsRequired)))
  )


  #correct column names
  names(heatmapAnno)[names(heatmapAnno) == 'df_category1_name'] <-category1_name
  names(heatmapAnno)[names(heatmapAnno) == 'df_category2_name'] <-category2_name

  heatmapAnno

  rectValues <- dplyr::left_join(heatmapAnno,
                                 .lts_output$lts_CCFcalcs[c("meanCorrAtModeMaxLAG", category1_name, category2_name)],
                                 by = c(category1_name, category2_name))
  unq_rectValues <- unique(rectValues)
  #join unique react values to column names

  lts_heatmapAnno <- dplyr::left_join(heatmapAnno, unq_rectValues[c("meanCorrAtModeMaxLAG", category1_name, category2_name)], by = c(category1_name, category2_name))
  lts_heatmapAnno



  # str(lts_heatmapAnno)

  unique(rectValues)


  # lts_variables$lts_compare_by[1] = rep(length(levels(lts_variables$lts_data[lts_variables$lts_compare_by[1]]*levels(lts_variables$lts_data[lts_variables$lts_compare_by[2]]))))


  lts_heatmapAnno$xmin <- as.numeric(lts_heatmapAnno$xmin)
  lts_heatmapAnno$xmax<- as.numeric(lts_heatmapAnno$xmax)
  lts_heatmapAnno$ymin<- as.numeric(lts_heatmapAnno$ymin)
  lts_heatmapAnno$ymax<- as.numeric(lts_heatmapAnno$ymax)
  lts_heatmapAnno[category1_name] <-  factor(lts_heatmapAnno[,.lts_output$lts_variables$lts_compare_by[[1]] ], levels = category1_levels)
  lts_heatmapAnno[category2_name] <-  factor(lts_heatmapAnno[,.lts_output$lts_variables$lts_compare_by[[2]] ], levels = category2_levels)
  str(lts_heatmapAnno)

  heatmapLagZero <- ggplot()+
    geom_rect(data=lts_heatmapAnno,
              aes(ymin=-Inf, ymax=Inf,
                  xmin=-Inf, xmax=Inf,
                  fill= meanCorrAtModeMaxLAG), alpha =0.5)+
    geom_line(data = subset_sum_join_outputCCFdata, aes(x =theLAG,
                                                        y = theCCF,
                                                        group = !!sym(.lts_output$lts_variables$lts_uniqueID_colname)), alpha = 0.5, color = "black")+  ##fix this instance of keynum
    scale_color_viridis_c(option ="magma")+
    scale_fill_viridis_c(option ="magma")+
    facet_grid( vars(!!sym(category2_name)), vars(!!sym(category1_name)) )+
    stat_summary(data = subset_sum_join_outputCCFdata,aes(x = theLAG, y = theCCF,group=1), fun=mean, colour="darkorange", geom="line",group=1, size = 1)+
    theme_classic() +
    theme(legend.position="bottom")+
    # + ylab() +
    theme(strip.text.y.right = element_text(angle = 0)) #remove this if a problem

  heatmapLagZero

  compoundPlot <- ggpubr::ggarrange(plt_dendr, heatmapLagZero + rremove("x.text"),
                                    heights = c(2, 7),
                                    align = "v",
                                    # labels = c("A", "B", "C"),
                                    ncol = 1, nrow = 2)

  return(eval(ensym_plotType))
}


