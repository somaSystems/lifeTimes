#install devtools if needed
if(!require("devtools")) install.packages("devtools")

#load devtools
library(devtools)

#install from github, using package access token
install_github("somaSystems/lifeTimes", auth_token = "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")

library(lifeTimes)





lts <- lts_in() #calculate cross correlation

lts_plot_ccfs(lts) #plot clustered correlations

lts_plot_ClustSum(lts) #plot direction of correlations

lts_plot_coupled(lts) # plot strength and direction of correlation

str(lts_catchmentsAndRivers)
str(lts_extractedCells)
