##' Compare vectors and report discrepancies
##'
##' Compare vectors and report discrepancies via stop(). This is useful
##' at the beginning of a pipeline, where it is important to check whether
##' critical variables/inputs have been defined. 
##' 
##' @param need vector of elements. Call the function specifying all
##' the required variables here.
##' @param have vector of elements. Call the function passing ls() here.
##' @export
checkVarsStop = function(have, need) {
  ## compare the need and have vectors to check what is missing
  missing = need[!(need %in% have)]
  if (length(missing)>0) {
    cat("Error: Undefined variables:\n");
    cat(paste(missing, collapse="\n"), "\n")
    stop("Specify above variables and try again", call. = FALSE);
  }  
  ## if reached here, all the needed variables exists, so nothing to do
}

