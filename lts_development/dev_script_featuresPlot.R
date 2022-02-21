lts<- lts_in()

colnames(lts_extractedCells)

lts_pairs <- lts_pairsMaker(colnames(lts_extractedCells[,c(1:3)]))

lts_pairs

lts <- lts_in(.in_tsData = lts_extractedCells,
       .in_compare_categorical = "Treatment",
       .in_time = "runNumber",
       .in_plot_measured_variables = TRUE,
       .in_pairedComparisons = lts_pairs,
       .in_uniqueID_colname = "cellNumber",
       .in_metaData = NULL )
