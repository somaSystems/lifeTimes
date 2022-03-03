#Convert garmin data

#option 1 https://www.r-bloggers.com/2020/11/converting-xml-data-to-r-dataframes-with-xmlconvert/
#xml convert

#option 2 https://cran.r-project.org/web/packages/trackeR/vignettes/TourDetrackeR.html
#TCX convert
if(!require("trackeR")){install.packages("trackeR")}

install.packages("trackeR") #install software


library("trackeR")

filepath <- system.file("extdata/tcx/", "2013-06-01-183220.TCX.gz", package = "trackeR")

runDF <- readTCX(file = filepath, timezone = "GMT")

str(runDF)

runTr0 <- trackeRdata(runDF)

runTr1 <- read_container(filepath, type = "tcx", timezone = "GMT")

identical(runTr0, runTr1)

data("runs", package = "trackeR")

plot(runs, session = 1:7)

plot(runs, session = 8, what = c("altitude", "pace"))


tryCatch(plot_route(runs, session = 1, source = "stamen"),
         error = function(x) "Failed to donwload map data")

tryCatch(leaflet_route(runs, session = c(1, 6, 12)),
         error = function(x) "Failed to donwload map data")

summary(runs, session = 1, moving_threshold = c(cycling = 2, running = 1, swimming = 0.5))


runs_summary <- summary(runs)
plot(runs_summary, group = c("total", "moving"),
     what = c("avgSpeed", "distance", "duration", "avgHeartRate"))



timeline(runs_summary)

run_zones <- zones(runs[1:4], what = "speed", breaks = c(0, 2:6, 12.5))
plot(run_zones)


wexp <- Wprime(runs, session = 11, quantity = "expended", cp = 4, version = "2012")
plot(wexp, scaled = TRUE)

d_profile <- distribution_profile(runs, session = 1:4, what = "speed",
                                  grid = list(speed = seq(0, 12.5, by = 0.05)))
plot(d_profile, multiple = TRUE)



c_profile <- concentrationProfile(d_profile, what = "speed")
plot(c_profile, multiple = TRUE, smooth = TRUE)

runsT <- threshold(runs)
dp_runs <- distribution_profile(runsT, what = "speed")
dp_runs_S <- smoother(dp_runs)
cp_runs <- concentration_profile(dp_runs_S)
plot(cp_runs, multiple = TRUE, smooth = FALSE)

cpPCA <- funPCA(cp_runs, what = "speed", nharm = 4)

round(cpPCA$varprop, 2)
#> [1] 0.66 0.25 0.06 0.02
plot(cpPCA, harm = 1:2)



## plot scores vs summary statistics
scoresSP <- data.frame(cpPCA$scores)
names(scoresSP) <- paste0("speed_pc", 1:4)
d <- cbind(runs_summary, scoresSP)

library("ggplot2")
## pc1 ~ session duration (moving)
ggplot(d) + geom_point(aes(x = as.numeric(durationMoving), y = speed_pc1)) + theme_bw()
## pc2 ~ avg speed (moving)
ggplot(d) + geom_point(aes(x = avgSpeedMoving, y = speed_pc2)) + theme_bw()
