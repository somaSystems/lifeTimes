#Convert garmin data

#option 1 https://www.r-bloggers.com/2020/11/converting-xml-data-to-r-dataframes-with-xmlconvert/
#xml convert

#option 2 https://cran.r-project.org/web/packages/trackeR/vignettes/TourDetrackeR.html
#TCX convert
if(!require("trackeR")){install.packages("trackeR")}

install.packages("trackeR") #install software


library("trackeR")

filepath <- system.file("extdata/tcx/", "2013-06-01-183220.TCX.gz", package = "trackeR")

cbgd <- readTCX(file = "data-raw/garmin_data/activity_1858059346.tcx", timezone = "GMT")

runDF <- cbgd

str(runDF)


runTr0 <- trackeRdata(runDF)

str(runTr0)
View(runTr0)

data(runTr0)


# runTr1 <- read_container(filepath, type = "tcx", timezone = "GMT")

# # identical(runTr0, runTr1)
# ?data()
# data("runs", package = "trackeR")

with(runTr0, plot(runs, session = 1:7))

plot(runs, session = 1:7)

plot(runs, session = 8, what = c("altitude", "pace"))

