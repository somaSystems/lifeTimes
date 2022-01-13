## code to prepare `DATASET` dataset goes here

lts_catchmentsAndRivers <- read.csv(file="data-raw/key_tidy_candr.csv")

lts_makeDefault_lifeTimesInput <- function(.tsData = lts_catchmentsAndRivers,
                               .time = c("dayOfseason"),
                               .compare_categorical = c("season","catchmentRegion"), #Categorical variables
                               .pairedComparisons = list(
                                 pair_1 =list(x = "rainfall_cm", y = "flow_m3s")), #pairedVarCCF
                               .uniqueID_colname = "key_num",
                               .metaData = NULL) {

  lts_variables <- list(lts_data = .tsData, #create list of variables
                        lts_time = .time,
                        lts_compare_by = .compare_categorical,
                        lts_pariedComparisons = .pairedComparisons,
                        lts_uniqueID_colname = .uniqueID_colname,
                        lts_metaData = .metaData)


  lts_variables$lts_data[,lts_variables$lts_compare_by ] <- lapply(   lts_variables$lts_data[,lts_variables$lts_compare_by ] , factor) #make compare_by variables, as factors

  return(lts_variables)}

lts_defaultVariables <- lts_makeDefault_lifeTimesInput()

usethis::use_data(lts_defaultVariables,lts_catchmentsAndRivers, internal = TRUE, overwrite = TRUE)

