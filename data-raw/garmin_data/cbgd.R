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

#heart_rate
#Speed
#altitude
#distance
#cadence_cycling
#power

#get delta of each


colnames(runDF)

#make categorical variables
#split time into hours
#make new row called column number

runDF <-
runDF$hour <-




str(runDF)


runTr0 <- trackeRdata(runDF)



str(runTr0)
View(runTr0)

data(runTr0)

plot(runDF$time)
head(runDF$time)



#Data looks like 1 event from 2017
#16th of the seventh 2017
#from ~9 in the morning to ~ 16:00 in the afternoon

tail(runDF$latitude)
tail(runDF$longitude)

51.48525
0.1803356
# runTr1 <- read_container(filepath, type = "tcx", timezone = "GMT")

# # identical(runTr0, runTr1)
# ?data()
# data("runs", package = "trackeR")

with(runTr0, plot(runs, session = 1:7))

plot(runs, session = 1:7)

plot(runs, session = 8, what = c("altitude", "pace"))

