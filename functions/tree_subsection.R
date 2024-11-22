#' selection of a subset of trees from a ranger object
#' @param rf        random forest object build with ranger
#' @param indices   vector of indices of selected trees
#' function returns: reduced ranger object with subset of selected trees


tree_subsection <- function(rf, indices, ...){
  # build ranger object that contains only the selected subset of trees
  rf_rep <- rf
  num.trees <- as.double(length(indices))
  rf_rep$num.trees <- num.trees
  rf_rep$forest$num.trees <- num.trees
  rf_rep$forest$child.nodeIDs <- rf_rep$forest$child.nodeIDs[indices]
  rf_rep$forest$split.varIDs  <- rf_rep$forest$split.varIDs[indices]
  rf_rep$forest$split.values  <- rf_rep$forest$split.values[indices]

  if(length(rf$inbag.counts) > 0){
    rf_rep$inbag.counts <- rf_rep$inbag.counts[indices]
  }

  return(rf_rep)
}