# if(!require(testthat)){
#   install.packages("testthat")
# }
#
#install devtools if needed
if(!require("devtools")) install.packages("devtools")

#load devtools
library(devtools)

#install dependency from github
install_github("jokergoo/ComplexHeatmap")

#install lifeTimes from github, using package access token
install_github("somaSystems/lifeTimes",
               auth_token = "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")
