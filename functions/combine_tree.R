#' combination of two random forests (both build with ranger)
#' @param rf1 random forest 1 (build with ranger)
#' @param rf2 random forest 2 (build with ranger)
#' function returns: combined ranger object

combine_tree <- function(rf1, rf2, ...){
  rf_rep <- rf1
  rf_rep$num.trees <- rf1$num.trees + rf2$num.trees
  rf_rep$forest$num.trees <- rf1$num.trees + rf2$num.trees
  rf_rep$forest$child.nodeIDs <- append(rf_rep$forest$child.nodeIDs, 
                                        rf2$forest$child.nodeIDs)
  rf_rep$forest$split.varIDs  <- append(rf_rep$forest$split.varIDs,
                                        rf2$forest$split.varIDs)
  rf_rep$forest$split.values  <- append(rf_rep$forest$split.values,
                                        rf2$forest$split.values)
  
  
  rf_rep$predictions      <- NULL
  rf_rep$prediction.error <- NULL
  
  return(rf_rep)
}