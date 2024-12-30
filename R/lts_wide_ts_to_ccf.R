#' lts_wide_ts_to_ccf
#' @import magrittr
#' @importFrom magrittr "%>%"
#' @param .lts_cast_ts Output from lts_ts_to_wide()
#' @param .lts_variables Output from lts_input()
#'
#' @return Returns a list of cross-correlation results, matching the original structure
#'

lts_wide_ts_to_ccf <- function(.lts_cast_ts = NULL, .lts_variables = NULL) {

  # Start timing
  start_time <- Sys.time()

  # Validate inputs
  if (is.null(.lts_cast_ts)) stop("Input .lts_cast_ts is required.")
  if (is.null(.lts_variables)) stop("Input .lts_variables is required.")
  if (is.null(.lts_variables$lts_pariedComparisons)) stop("No paired comparisons provided.")

  # Extract variables and prepare inputs
  .pairedComparisons <- .lts_variables$lts_pariedComparisons
  .uniqueID_colname <- .lts_variables$lts_uniqueID_colname
  lts_lagMax <- .lts_variables$lts_lagMax

  # Extract unique IDs
  unique_ids <- unique(.lts_cast_ts %>% dplyr::select(starts_with(.uniqueID_colname)))

  # Helper function to calculate CCF for a single pair and ID
  calculate_ccf <- function(id, pair) {
    y_col <- grep(paste0("^", id, "/", pair[[1]]), names(.lts_cast_ts), value = TRUE)
    x_col <- grep(paste0("^", id, "/", pair[[2]]), names(.lts_cast_ts), value = TRUE)

    if (length(y_col) != 1 || length(x_col) != 1) {
      warning(paste("Skipping ID", id, "and pair", paste(pair, collapse = " vs "), "due to missing data."))
      return(NULL)
    }

    y <- .lts_cast_ts[[y_col]]
    x <- .lts_cast_ts[[x_col]]

    # Perform cross-correlation
    ccf_result <- stats::ccf(y, x, plot = FALSE, na.action = na.pass, lag.max = lts_lagMax)

    # Create output in the required format
    list(
      theCCF = ccf_result$acf,
      theLAG = ccf_result$lag,
      lts_uniqueID_colname = rep(id, length(ccf_result$lag)),
      theFeature = as.factor(rep(paste(pair[[1]], "vs", pair[[2]], sep = " "), length(ccf_result$lag)))
    )
  }

  # Flatten combinations of unique IDs and paired comparisons
  all_combinations <- expand.grid(
    id = unique_ids,
    pair = .pairedComparisons,
    stringsAsFactors = FALSE
  )

  # Parallel processing using explicit namespace
  lts_ccf_list <- future.apply::future_lapply(seq_len(nrow(all_combinations)), function(i) {
    combo <- all_combinations[i, ]
    calculate_ccf(combo$id, combo$pair)
  })

  # Name the list elements
  names(lts_ccf_list) <- paste0("CCF_", seq_along(lts_ccf_list))

  # Filter out NULL results
  lts_ccf_list <- lts_ccf_list[!sapply(lts_ccf_list, is.null)]

  # End timing
  end_time <- Sys.time()
  elapsed_time <- difftime(end_time, start_time, units = "secs")
  cat("Total computation time:", as.numeric(elapsed_time), "seconds\n")

  return(lts_ccf_list)
}
