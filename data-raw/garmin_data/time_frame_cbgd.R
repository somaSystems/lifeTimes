#Convert garmin data

#option 1 https://www.r-bloggers.com/2020/11/converting-xml-data-to-r-dataframes-with-xmlconvert/
#xml convert

#option 2 https://cran.r-project.org/web/packages/trackeR/vignettes/TourDetrackeR.html
#TCX convert
if(!require("trackeR"))(install.packages("trackeR"))

# install.packages("trackeR") #install software


library("trackeR")

# filepath <- system.file("extdata/tcx/", "2013-06-01-183220.TCX.gz", package = "trackeR")

runDF <- readTCX(file = "data-raw/garmin_data/activity_1858059346.tcx", timezone = "GMT")

# runDF <- cbgd

str(runDF)

#heart_rate
#Speed
#altitude
#distance
#cadence_cycling
#power

#unique observation to be compared to others is a 2 minute block
#Categorical variables are beginning or end of day

#also do for 30 minute blocks


str(runDF$time)

runDF$time_num <- as.numeric(runDF$time)


runDF$time_num
runDF$time_num_zero <- runDF$time_num - (min(runDF$time_num))
runDF$time_num_zero


max(runDF$time_num_zero)

#check for missing seconds
length(runDF$time_num_zero) # length less than max so there are missing times

#find missing times

time_num_zero <- seq(from = 0, to =max(runDF$time_num_zero))

time_num_zero
time_num_zero_df <- as.data.frame(time_num_zero)

length(time_num_zero)
colnames(time_num_zero)

runDF <- dplyr::left_join(time_num_zero_df, runDF, by = c("time_num_zero"))

# time_frame

#make time reset ever 120 seconds to give two minute sessions
#these sessions are replicates

runDF$time_num_zero_twoMinReset <- (runDF$time_num_zero) - (floor(runDF$time_num_zero/120) * 120)
runDF$time_num_zero_twoMinReset

# runDF <- time_frame
# View(runDF)

unique(runDF$time_num_zero_twoMinReset)

floor(max(runDF$time_num_zero)/120) #get number of 120 second units
trim_runDF <- runDF[c(0:(floor(max(runDF$time_num_zero)/120)*120)),] #trim data to fit into two minute units


#https://www.statology.org/r-create-categorical-variable-from-continuous/
#cut runDF into 2 minute categroical variables
nrow(trim_runDF)
twoMinBreaks <- c(seq(0, nrow(trim_runDF), by=120, ))

# key_twoMinBreaks <- paste0("key_",twoMinBreaks[-length(twoMinBreaks)]/120) # make vector one less than number of breaks
key_twoMinBreaks <- paste0("key_",twoMinBreaks/120) # make vector one less than number of breaks

length(twoMinBreaks)
length(key_twoMinBreaks)
key_twoMinBreaks
cut_trim_runDF <- cut(trim_runDF$time_num_zero,
                      breaks =twoMinBreaks,
                      labels = key_twoMinBreaks[-1], #remove first label in key
                      include.lowest = TRUE)

cut_trim_runDF
# cut_t
length(cut_trim_runDF)
# View(cut_trim_runDF)

#Join two minute intervals back to main data

trim_runDF_twoMins <- cbind(trim_runDF, cut_trim_runDF)
nrow(trim_runDF_twoMins)
head(trim_runDF)

# split data into quarters

session_fifths <- cut(trim_runDF$time_num_zero,
                      breaks = 5,
                      labels = c("first_session","second_session","third_session","fourth_session","fifth_session"))




length(session_fifths)

#Join session fifths back to original
trim_runDF_twoMins_session <- cbind(trim_runDF_twoMins, session_fifths)
nrow(trim_runDF_twoMins_session)

# View(trim_runDF_twoMins_session)

tail(trim_runDF_twoMins_session$time_num_zero_twoMinReset,1000)
head(trim_runDF_twoMins_session$time_num_zero_twoMinReset,400)

head(trim_runDF)

#Impute values

trim_runDF_twoMins_session$time_num_zero_char <- paste0("char",trim_runDF_twoMins_session$time_num_zero)


library(purrr)
library(imputeTS)
library(dplyr)
trim_runDF_impute <- trim_runDF_twoMins_session %>%
  # filter()
  dplyr::select((where(is.numeric)))%>%
  # group_by(c_IDcellNumber_frame, geomFeature)%>%
  purrr::map_dfc(~na_ma(.x,k=1)) # how to only do this on is.numeric without previous step


nrow(trim_runDF_impute)



non_numeric <- trim_runDF_twoMins_session %>%
  dplyr::select(!(where(is.numeric)))


nrow(non_numeric)

runDF_imputed <- cbind(non_numeric,trim_runDF_impute) # join imputed and categorical data


colnames(runDF_imputed)
head(runDF_imputed)

#select data columns
sub_runDF_imp <- runDF_imputed %>%
  dplyr::select(time,
                time_num_zero_char,
                cut_trim_runDF,
                session_fifths,
                time_num_zero,
                time_num_zero_twoMinReset,
                latitude,
                longitude,
                altitude,
                distance,
                heart_rate,
                speed,
                cadence_cycling,
                power,
                time_num_zero_twoMinReset)



#Create difference columns
diff_runDF_imputed <- sub_runDF_imp %>%
  arrange(runDF_imputed$time_num)%>%
  # group_by(an_CCF_ObjectID, an_CCF_Feature,Treatment) %>%
  mutate_if(is.numeric, ~ . - dplyr::lag(., n = 1, default = NA),.keep = "all" )%>%
  rename_with((where(is.numeric)), .fn = ~ paste0("diff_", .x))

diff_runDF_imputed

mtcars %>% rename_with(.fn = ~ paste0("Myprefix_", .x, "_Mypostfix")) -> mtcars.custom


diff_runDF_imputed

varList<- names(diff_runDF_imputed)[!(names(diff_runDF_imputed) %in% names(sub_runDF_imp))] # get non common names
varList<- c(varList,"time_num_zero_char") # appending key parameter

#join original time to differenced time
raw_and_diff <- dplyr::left_join(sub_runDF_imp,diff_runDF_imputed[,varList], by =(c("time_num_zero_char")))


# View(raw_and_diff)

# raw_and_diff



#check every twoMinute ID has 120 timepoints

sum_IDs <- raw_and_diff %>%
  group_by(cut_trim_runDF)%>%
  tally()

max(sum_IDs$n)
min(sum_IDs$n)

#get list to keep

key_IDs_to_keep <- sum_IDs[sum_IDs$n == 120,]

key_IDs_to_keep <- key_IDs_to_keep$cut_trim_runDF

key_IDs_to_keep

filt_raw_and_diff <- raw_and_diff %>%
  dplyr::filter(cut_trim_runDF %in% key_IDs_to_keep)  # filter out things based on list of correct time intervals

# filt_raw_and_diff


# raw_and_diff


filt_raw_and_diff$unq_key <- paste0(filt_raw_and_diff$cut_trim_runDF,"-",filt_raw_and_diff$session_fifths)


find_dup_keys <- unique(filt_raw_and_diff$unq_key)


split_find_dup_keys <- strsplit(find_dup_keys, "-")

str(split_find_dup_keys)

dup_keys <- do.call(rbind, split_find_dup_keys)

dup_keys_df <- as.data.frame(dup_keys)

dups_to_remove <- dup_keys_df[duplicated(dup_keys_df$V1),]$V1

dups_to_remove

#check after
RowsToRemove <- grep(paste(dups_to_remove, collapse="|"), filt_raw_and_diff$cut_trim_runDF)

RowsToRemove

filt_raw_and_diff_duprm <- filt_raw_and_diff[-RowsToRemove,] #remove obs spanning two sessions these make NAs later

View(filt_raw_and_diff_duprm)




filt_raw_and_diff_duprm$cut_trim_runDF


# filt_raw_and_diff_duprm <- filt_raw_and_diff %>%
#   dplyr::filter(grepl(paste(v1, collapse="|"), dups_to_remove))



##Find observations that span sessions and remove them



# colnames(filt_raw_and_diff)
#
# filt_raw_and_diff$cut_trim_runDF
#
# checkForDuplicates_twoMinAndSessions <- (filt_raw_and_diff[,c("cut_trim_runDF","session_fifths")])
#
# Dup_keys<- checkForDuplicates_twoMinAndSessions$cut_trim_runDF
#
# Dup_keys[duplicated(Dup_keys)]

filt_raw_and_diff_duprm$cut_trim_runDF


unqIDtally <- filt_raw_and_diff_duprm%>%
  group_by(unq_key)%>%
  tally()


max(unqIDtally$n)
min(unqIDtally$n)

write.csv(filt_raw_and_diff_duprm,"lts_garmin_data.csv")

colnames(raw_and_diff)


nrow(trim_runDF_impute)
nrow(non_numeric)



#As above, but Do not impute first time point

wide_group_impute_groupCellTime






#unique ID for each two minute interval is key_num
#categorical variable is session fifths
#time is time_num_zero
#colums of inteerest are
#lat
#long
#altitude
#function to impute NA


#trim make wide

library(tidyr)

colnames(trim_runDF)
# trim_runDF$
sub_trim_runDF <- trim_runDF %>%
  dplyr::select(time_num_zero_twoMinReset,
                latitude,
                # session_fifths,
                longitude,
                altitude,
                distance,
                heart_rate,
                speed,
                cadence_cycling,
                power,
                cut_trim_runDF)

colnames(sub_trim_runDF)


long_runDF <- pivot_longer(sub_trim_runDF,
             cols = !(contains("time_num_zero_twoMinReset") |
                      contains("cut_trim_runDF") |
                      contains("session_fifths")),
             names_to = "session_measures"
             )
long_runDF


wide_runDF <- pivot_wider(long_runDF,
            id_cols = time_num_zero_twoMinReset,
            values_from = value,
            names_from = c(cut_trim_runDF, session_measures),
            names_sep = "/",
            values_fn = length)

wide_runDF

str(wide_runDF)



apply(wide_runDF[-1],1, function(x) sum(x, na.rm = TRUE) > length(wide_runDF[-1])) # find where duplicates are

# View(wide_runDF)


?apply()

library(dplyr)
library(imputeTS)

#make wide and impute








rundDF$key_time2min <- cut(runDF$time,
                           breaks)

library(lubridate)
library(dplyr)
runDF$time

mdy_hms(runDF$time)

twoMinInt <- interval(runDF$time, runDF$time + minutes(2))

runDF$time_2min <- twoMinInt
colnames(runDF)
head(runDF[,c("time","time_2min")])



# mdy_h

out <- runDF %>%
  mutate(time_mdy = mdy_hms(time),
         day = day(time),
         month = month(time),
         year = year(time),
         dayofweek = wday(time),
         minute = minute(time),
         second = second(time))

#get delta of each


colnames(runDF)

#make categorical variables
#split time into hours
#make new row called column number

# runDF <-
# runDF$hour <-




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

