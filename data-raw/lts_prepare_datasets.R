## Code to prepare datasets for the `lifeTimes` package

# Step 1: Load raw data from `data-raw/`
lts_catchmentsAndRivers <- read.csv(file = "data-raw/key_tidy_candr.csv")
lts_extractedCells <- read.csv(file = "data-raw/extractedCells.csv")

# Step 2: Function to create default variables for the package
lts_makeDefault_lifeTimesInput <- function(
    .tsData = lts_catchmentsAndRivers,
    .time = c("dayOfseason"),
    .compare_categorical = c("season", "catchmentRegion"), # Categorical variables
    .pairedComparisons = list(
      pair_1 = list(x = "rainfall_cm", y = "flow_m3s") # Paired variables for CCF
    ),
    .uniqueID_colname = "key_num",
    .metaData = NULL
) {
  # Create list of variables for lifeTimes input
  lts_variables <- list(
    lts_data = .tsData,
    lts_time = .time,
    lts_compare_by = .compare_categorical,
    lts_pariedComparisons = .pairedComparisons,
    lts_uniqueID_colname = .uniqueID_colname,
    lts_metaData = .metaData
  )

  # Convert categorical variables to factors
  lts_variables$lts_data[, lts_variables$lts_compare_by] <- lapply(
    lts_variables$lts_data[, lts_variables$lts_compare_by],
    factor
  )

  return(lts_variables)
}

# Step 3: Create default variables
lts_defaultVariables <- lts_makeDefault_lifeTimesInput()

# Step 4: Save datasets to the `data/` directory
usethis::use_data(
  lts_catchmentsAndRivers,
  lts_extractedCells,
  lts_defaultVariables,
  overwrite = TRUE
)

