#Refactor garminData import

if(!require("trackeR"))(install.packages("trackeR"))
library("trackeR")
runDF <- readTCX(file = "data-raw/garmin_data/activity_1858059346.tcx", timezone = "GMT")

#Make time numeric
runDF$time_num <- as.numeric(runDF$time)
runDF$time_num_zero <- (runDF$time_num - (min(runDF$time_num))) # count time from 1 second

runDF$time_num_zero

#some time points are missing
#find these
# max(runDF$time_num_zero)
one_second_time_frame <- seq(from = 0, to =max(runDF$time_num_zero))
# head(one_second_time_frame)

one_second_time_frame <- as.data.frame(one_second_time_frame)
#make sure to cut out at max of time_num_zero
 head(one_second_time_frame)

 # ?left_join()

join_runDF <- dplyr::left_join(one_second_time_frame, runDF, by = c("one_second_time_frame"="time_num_zero"), keep = TRUE)

join_runDF$one_second_time_frame
join_runDF$one_second_time_framePLUSONE <- join_runDF$one_second_time_frame +1
join_runDF$time_num_zero
# head(runDF)


#make time reset ever 120 seconds to give two minute sessions
#these sessions are replicates
# runDF$one_second_time_frame
join_runDF$tzero_twoMinSets <- (join_runDF$one_second_time_frame) - (floor(join_runDF$one_second_time_frame/120) * 120)
join_runDF$tzero_twoMinSets <- join_runDF$tzero_twoMinSets +1 #add one to make up the seconds from 1 to 120
join_runDF$tzero_twoMinSets

twoMinBreaks <- c(seq(from = 0, to = nrow(join_runDF), by=120 )) #This by 120 is needed to give 120

# twoMinBreaks

cut_trim_runDF <- cut(join_runDF$one_second_time_framePLUSONE,
                      breaks =twoMinBreaks,
                      labels = key_twoMinBreaks[-1], #remove first label in key
                      include.lowest = TRUE) # in comination with from 0 (lowest is 1)

length(cut_trim_runDF)
nrow(join_runDF)

trim_runDF_twoMins <- cbind(join_runDF, cut_trim_runDF)

# View(trim_runDF_twoMins)

# unique(trim_runDF_twoMins$cut_trim_runDF)
#To avoid splitting two minute intervals in two, split the breaks along the unique list of two minute intervals
session_fifths <- cut(unique(as.numeric(trim_runDF_twoMins$cut_trim_runDF)),
                      breaks = 5,
                      labels = c("first_session","second_session","third_session","fourth_session","fifth_session"))

# make a key to join the sessions and two minute periods

twoMinSession_lookup <- cbind(unique(as.character(trim_runDF_twoMins$cut_trim_runDF)),
                              as.character(session_fifths))

twoMinSession_lookup_df <- as.data.frame(twoMinSession_lookup)

twoMinSession_lookup_df

# session_fifths

runDF_twoMins_sessions <- dplyr::left_join(trim_runDF_twoMins, twoMinSession_lookup_df, by = c("cut_trim_runDF"="V1"))

runDF_twoMins_sessions <- runDF_twoMins_sessions%>%
  dplyr::rename("session_split" = V2)


#impute missing values

#Impute values

# trim_runDF_twoMins_session$time_num_zero_char <- paste0("char",trim_runDF_twoMins_session$time_num_zero)

# https://stackoverflow.com/questions/60235794/map-as-numeric-to-only-specific-columns-of-a-dataframe
library(purrr)
library(imputeTS)
library(dplyr)
impute_runDF_twoMins_sessions <- runDF_twoMins_sessions %>%
  # filter()
  dplyr::select(latitude, # select measured values
                longitude,
                altitude,
                distance,
                heart_rate,
                speed,
                cadence_cycling,
                power)%>%
  # group_by(c_IDcellNumber_frame, geomFeature)%>%
  purrr::map_dfc(~na_ma(.x,k=1)) # how to only do this on is.numeric without previous step


# nrow(impute_runDF_twoMins_sessions)
# colnames(impute_runDF_twoMins_sessions)

non_imputed_runDF_twoMins_sessions <- runDF_twoMins_sessions %>%
  dplyr::select(-latitude, # select measured values
                                -longitude,
                                -altitude,
                                -distance,
                                -heart_rate,
                                -speed,
                                -cadence_cycling,
                                -power,
                -cadence_running,
                - temperature) # drop running and temp as they are empty

colnames(non_imputed_runDF_twoMins_sessions)

# nrow(non_numeric_runDF_twoMins_sessions)

nrow(non_imputed_runDF_twoMins_sessions)
nrow(impute_runDF_twoMins_sessions)

join_garmin_data_imputed <- cbind(non_imputed_runDF_twoMins_sessions, impute_runDF_twoMins_sessions) # rejoin imputed measures and metadata/time measures

colnames(join_garmin_data_imputed)



join_garmin_data_imputed <- join_garmin_data_imputed %>%
  dplyr::rename( unq_key_garmin = "cut_trim_runDF",
                 two_min_time = "tzero_twoMinSets")


write.csv(join_garmin_data_imputed,"cleaned_garmin.csv")


################END######################
runDF_imputed <- cbind(non_numeric,trim_runDF_impute) # join imputed and categorical data



# runDF_twoMins_sessions[] <- purrr::map_at(runDF_twoMins_sessions, numeric_variables, as.numeric)
# df


# runDF_twoMins_sessions

# trim_runDF_twoMins_sessions

# length(session_fifths)

# breaksCheck <- runDF_twoMins_sessions[,c("one_second_time_framePLUSONE","cut_trim_runDF","session_split")]

# View(breaksCheck)

# head(cut_trim_runDF,500)

# (join_runDF)
# runDF <- time_frame
# View(runDF)

# unique(runDF$time_num_zero_twoMinReset)

# floor(max(join_runDF$one_second_time_frame)/120) #get number of 120 second units
# trim_runDF <- runDF[c(0:(floor(max(runDF$time_num_zero)/120)*120)),] #trim data to fit into two minute units


twoMinBreaks

# key_twoMinBreaks <- paste0("key_",twoMinBreaks[-length(twoMinBreaks)]/120) # make vector one less than number of breaks
key_twoMinBreaks <- paste0("key_",(twoMinBreaks/120)) # make vector one less than number of breaks
# short_key_twokey_twoMinBreaks[-1]

length(key_twoMinBreaks)
length(twoMinBreaks)

cut_trim_runDF <- cut(join_runDF$one_second_time_frame,
                      breaks =twoMinBreaks,
                      labels = key_twoMinBreaks[-1], #remove first label in key
                      include.lowest = TRUE)

trim_runDF_twoMins <- cbind(trim_runDF, cut_trim_runDF)

