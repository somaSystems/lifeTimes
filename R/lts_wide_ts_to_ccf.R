#' lts_wide_ts_to_ccf
#' @import magrittr
#' @importFrom magrittr "%>%"
#' @importFrom magrittr %>%
#' @param .lts_cast_ts output from lts_ts_to_wide()
#' @param .lts_variables output from lts_input()
#'
#' @return
#'

lts_wide_ts_to_ccf <- function(.lts_cast_ts = NULL, .lts_variables = NULL) {


  #make default data if needed
  if(is.null(.lts_variables)){
    .lts_variables <- lts_defaultVariables
  }

  Lts_CCFunqVars <- as.data.frame(c(.lts_variables$lts_data[,c( .lts_variables$lts_uniqueID_colname, .lts_variables$lts_compare_by)]))
  Lts_CCFunqVars[.lts_variables$lts_compare_by] <- lapply(Lts_CCFunqVars[.lts_variables$lts_compare_by],  as.character) #select column "containers", not just contents, so no ","
  Lts_CCFunqVars$unqCCFtsLabel <- apply(Lts_CCFunqVars, 1, function(x) paste(x, collapse="_")) ###this is just for PRINTING
  lts_lagMax <- .lts_variables$lts_lagMax #hotfix July 27 2022

  .key_unqIDcombo <- unique(Lts_CCFunqVars[c("unqCCFtsLabel",.lts_variables$lts_uniqueID_colname)]) #make unique col name a variable
  .unqNameKey <- .key_unqIDcombo$unqCCFtsLabel
  .unqNumKey <-  .key_unqIDcombo[,.lts_variables$lts_uniqueID_colname]
  # } #hotfix no. 2, make this dynanimc, make sure to select contents (",")

  .pairedComparisons =   .lts_variables$lts_pariedComparisons

  # lts_ccf_list <- vector("list", ncol(.lts_cast_ts[-1])/2)  # 1. loop  output
  # lts_ccf_list <- vector("list", ncol(.lts_cast_ts[-1]))  # 1. new length for list creation, eg. 5 things to compare*250 cells gives 1250 columns, but each one --> gives 10 unique combination
  # make empty list based on number of observations and number of paired comparisons
  numberOfObjects <- length(.unqNumKey)
  numberOfComparisons <- length(.pairedComparisons)
  lts_ccf_list <- vector("list", numberOfObjects*numberOfComparisons) #number of observation * number of comparisons per cell
  print(paste("Generating empty list of:",length(lts_ccf_list))) #Jan2022
  element <- 1
  for(keyIndex in seq_along(.unqNumKey)){

    print(paste("element is", element))
    #get the index number of the key # hot fix number three, change this here
    key_name <- .unqNameKey[keyIndex] #get the actual name descriptive name of the key instead of index number
    .key_num <- .unqNumKey[keyIndex] #get the numerical non descriptive key index
    print(paste("The key_num is:", .key_num))

    for(pairIndex in seq_along(.pairedComparisons)){ #key contains place and season (compare by), so now just do all paired comparisons
      innerElement <- element
      print(paste("the pairINDEX is:", pairIndex))
      pair <- .pairedComparisons[[pairIndex]]
      # print(paste("The pair is:", "y=", pair$y," ..x=", pair$x ))
      print(paste("The pair is:", "y=", pair[[1]]," ..x=", pair[[2]] ))
      print(paste("Started adding...", key_name, paste(.pairedComparisons[[pairIndex]], collapse ="_vs_"), sep = "...")) #print stage of loop
      #this looks up by cell number and comparison
      chosenObs_y <- .lts_cast_ts[,grepl(c(paste0("^",.key_num,"/")), names(.lts_cast_ts)) & # gets column with key_num #hotfix feb 20 2022 add "^" to match keynum from start of string
                                    # grepl(c(pair$y), names(.lts_cast_ts))]  # also gets column with pair y
                                    # grepl(c(pastepair[[1]]), names(.lts_cast_ts))]  # also gets column with pair y

                                    grepl(c(paste0("/",pair[[1]])), names(.lts_cast_ts))]  # also gets column with pair y

      # print(paste("chosenObs_y:", .key_num, pair$y))
      print(paste("chosenObs_y:", .key_num, pair[[1]]))
      chosenObs_x <- .lts_cast_ts[,grepl(c(paste0("^",.key_num,"/")), names(.lts_cast_ts)) & # gets column with key_num #hotfix feb 20 2022 add "^" to match keynum from start of string
                                    # grepl(c(pair$x), names(.lts_cast_ts))] #sequence of 91 measures
                                    # grepl(c(pair[[2]]), names(.lts_cast_ts))] #sequence of 91 measures
                                    grepl(c(paste0("/",pair[[2]])), names(.lts_cast_ts))]  # also gets column with pair y

      # print(paste("chosenObs_x:", .key_num, pair$x))
      print(paste("chosenObs_x:", .key_num, pair[[2]]))
      instanceOfCCF <- stats::ccf(chosenObs_y, chosenObs_x, plot = FALSE, na.action = na.pass, lag.max=lts_lagMax) #calculate CCF for chosen pairing
      print("ccf complete")
      anCCF_ACF <- instanceOfCCF$acf #current CCF_correlation values
      anCCF_LAG <- instanceOfCCF$lag #current CCF set of lags
      an_CCF_ObjectID <- rep(.key_num, length(instanceOfCCF$lag)) # Object ID for current CCF
      # an_CCF_Feature <-  rep(paste(pair$y,"\n","versus","\n",pair$x, sep=" "), length(instanceOfCCF$lag)) # Feature name for current
      print("creating feature name")
      an_CCF_Feature <-  rep(paste(pair[[1]],"\n","versus","\n",pair[[2]], sep=" "), length(instanceOfCCF$lag)) # Feature name for current CCF
      print("feature name complete")

      #going for the separator

      instanceOfCCF_Object_output <- list(theCCF = anCCF_ACF ,
                                          theLAG = anCCF_LAG,
                                          lts_uniqueID_colname = an_CCF_ObjectID, #this creates literal names
                                          theFeature = as.factor(an_CCF_Feature)) # Bind these together as a "row" of a dataframe
      #hotfix1 moved the update list step outside of the loop, so paired comparison sets don't overwrite each other #KEY INDEX IS 1:4, not long enough
      print("generating instance complete")


      lts_ccf_list[[element]] <- instanceOfCCF_Object_output # add current output to main dataframe
      names(lts_ccf_list)[element] <- key_name
      print("adding instance complete")

      print(paste("Finished adding...",key_name, paste(.pairedComparisons[[pairIndex]], sep = "..."))) #print stage of loop
      element <- element+1


    }

  }
  return(lts_ccf_list)
}


