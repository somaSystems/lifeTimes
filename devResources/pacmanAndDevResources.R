#Package setup script

install.packages("pacman")

pacman::p_load(
  devtools,
  usethis,
  roxygen2,
  testthat,
  knitr,
  rmarkdown
)

#https://www.r-bloggers.com/2016/06/non-standard-evaluation-and-standard-evaluation-in-dplyr/
#https://github.com/Rdatatable/data.table/issues/850

#Used use_package("dplyr")
#To add packages to DESCRIPTION file
#
#Click in function definition and go to Code and insert roxygen skeleton
#to make documentation

#Then use document() function

# Conflicts ────────────────────────────────────────────────────────────────────────────────────────────────────────────────── lifeTimes conflicts ──
# x couplingPlot()       masks lifeTimes::couplingPlot()
# x defaultData()        masks lifeTimes::defaultData()
# x leadLagClusterPlot() masks lifeTimes::leadLagClusterPlot()
# … and 3 more
#
# Did you accidentally source a file rather than using `load_all()`?
#   Run `rm(list = c("couplingPlot", "defaultData", "leadLagClusterPlot", "lifeTimesChain", "metaData_ccf_join", "tsToWide"))` to remove the conflicts
