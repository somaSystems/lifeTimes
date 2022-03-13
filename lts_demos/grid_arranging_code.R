# https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html

library(ggplot2)
p1 <- qplot(mpg, wt, data = mtcars, colour = cyl)
p2 <- qplot(mpg, data = mtcars) + ggtitle("title")
p3 <- qplot(mpg, data = mtcars, geom = "dotplot")
p4 <-
  p1 + facet_wrap( ~ carb, nrow = 1) + theme(legend.position = "none") +
  ggtitle("facetted plot")

library(gridExtra)
grid.arrange(p1, p2, nrow = 1)

grid.arrange(
  grobs = gl,
  widths = c(2, 1, 1),
  layout_matrix = rbind(c(1, 2, NA),
                        c(3, 3, 4))
)

library(grid)
g <- ggplotGrob(qplot(1, 1) +
                  theme(plot.background = element_rect(colour = "black")))
qplot(1:10, 1:10) +
  annotation_custom(
    grob = g,
    xmin = 1,
    xmax = 5,
    ymin = 5,
    ymax = 10
  ) +
  annotation_custom(
    grob = rectGrob(gp = gpar(fill = "white")),
    xmin = 7.5,
    xmax = Inf,
    ymin = -Inf,
    ymax = 5
  )


library(gtable)
g2 <- ggplotGrob(p2)
g3 <- ggplotGrob(p3)
g <- rbind(g2, g3, size = "first")
g$widths <- unit.pmax(g2$widths, g3$widths)
grid.newpage()
grid.draw(g)

# install.packages("egg")
library(egg)
library(ggpubr)
p1 <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) +
  geom_point()+ theme_article() + theme(legend.position = 'top')
p2 <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) +
  geom_point() + facet_wrap(~ cyl, ncol = 2, scales = "free") +
  guides(colour = "none") +
  theme_article()
# p1
# p2
ggarrange(p1, p2, widths = c(1.5,2))


grid.arrange(
  p3,
  p3,
  p3,
  nrow = 1,
  top = "Title of the page",
  bottom = textGrob(
    "this footnote is right-justified",
    gp = gpar(fontface = 3, fontsize = 9),
    hjust = 1,
    x = 1
  )
)


grid_arrange_shared_legend <-
  function(...,
           ncol = length(list(...)),
           nrow = 1,
           position = c("bottom", "right")) {

    plots <- list(...)
    position <- match.arg(position)
    g <-
      ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
    legend <- g[[which(sapply(g, function(x)
      x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    lwidth <- sum(legend$width)
    gl <- lapply(plots, function(x)
      x + theme(legend.position = "none"))
    gl <- c(gl, ncol = ncol, nrow = nrow)

    combined <- switch(
      position,
      "bottom" = arrangeGrob(
        do.call(arrangeGrob, gl),
        legend,
        ncol = 1,
        heights = unit.c(unit(1, "npc") - lheight, lheight)
      ),
      "right" = arrangeGrob(
        do.call(arrangeGrob, gl),
        legend,
        ncol = 2,
        widths = unit.c(unit(1, "npc") - lwidth, lwidth)
      )
    )

    grid.newpage()
    grid.draw(combined)

    # return gtable invisibly
    invisible(combined)

  }

grid_arrange_shared_legend(p1, p2)


############
#https://cran.r-project.org/web/packages/egg/vignettes/Overview.html
library(grid)
p1 <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) +
  geom_point()

p2 <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) +
  geom_point() + facet_wrap(~ cyl, ncol = 2, scales = "free") +
  guides(colour = "none") +
  theme()

p3 <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) +
  geom_point() + facet_grid(. ~ cyl, scales = "free")

g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g3 <- ggplotGrob(p3)

fg1 <- gtable_frame(g1, debug = TRUE)
fg2 <- gtable_frame(g2, debug = TRUE)
fg12 <-
  gtable_frame(gtable_rbind(fg1, fg2),
               width = unit(2, "null"),
               height = unit(1, "null"))
fg3 <-
  gtable_frame(
    g3,
    width = unit(1, "null"),
    height = unit(1, "null"),
    debug = TRUE
  )
grid.newpage()
combined <- gtable_cbind(fg12, fg3)
grid.draw(combined)



#################### lts_ecxample
library(egg)
p1 <- plt_dendr

p2 <- heatmapLagZero

# p3 <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) +
#   geom_point() + facet_grid(. ~ cyl, scales = "free")

g1 <- ggplot2::ggplotGrob(p1)
g2 <- ggplot2::ggplotGrob(p2)
# g3 <- ggplotGrob(p3)

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
grid::grid.newpage()
grid::grid.draw(fg12)


?grid.newpage()
fg3 <-
  gtable_frame(
    g3,
    width = unit(1, "null"),
    height = unit(1, "null"),
    debug = TRUE
  )
grid.newpage()
combined <- gtable_cbind(fg12, fg3)
grid.draw(combined)
