#' lts_pairs
#' This function creates input to the ".lts_comparisons" argument of "lts_input". It takes a list of all the columns you are interested in and generates all possible non redundant, non self pairs.
#' @param cols a vector of column names to be compared
#'
#' @return returns a list of elements, where each element is a list of variables that will have CCFs calculated
#' @export
#'
#' @examples  lts_pairsMaker(c("pigeon","ferret","crab"))
#'
#'@importFrom gtools combinations

lts_pairsMaker <- function(cols = NULL){

# compareColsNames <- colnames(cols) #TODO, take colnums from dataframe and get names

lts_pairs  <- gtools::combinations(n = length(cols), #make unique and non self pairings of columns
                                   r =2, v = cols)

df_lts_pairs <- as.data.frame(lts_pairs) #make sets of pairs a dataframe

paired <- mapply(c, df_lts_pairs[[1]], df_lts_pairs[[2]], SIMPLIFY = FALSE) #make a list of sets of pairs

proto_listOflists <- vector("list",length(paired)) #make an empty list for holding lists of pairs
lts_listOflists <- lapply(paired, function(x) proto_listOflists[x] <- as.list(x)) #create lists of pairs

return(lts_listOflists)

}