#' lts_ccf_to_df
#'
#' @param .lts_ccflist output from lts_wide_ts_to_ccf()
#' @param .lts_variables output from lts_input()
#'
#' @importFrom magrittr "%>%"
#' @importFrom magrittr %>%
#'
#' @return ccfs in the form of a data frame

lts_ccf_df <- function(.lts_ccflist = lts_ccf_list, .lts_variables = NULL){


  if(is.null(.lts_variables)){
    # print(paste("not_assigned:",lts_defaultVariables))
    .lts_variables <- lts_defaultVariables
  }

  lts_protodf_list <- list(length(.lts_ccflist)) #define list

  for(key in seq_along(.lts_ccflist)){
    df_lts_obs <- as.data.frame((.lts_ccflist[[key]]))
    lts_protodf_list[[key]] <- df_lts_obs
  }

  lts_dfccf <- do.call(rbind, lts_protodf_list)

  names(lts_dfccf)[names(lts_dfccf) =="lts_uniqueID_colname"] <- .lts_variables$lts_uniqueID_colname

  return(lts_dfccf)
}
