##
## checks if variables (objects) are defined (using ls())
## 
## need - vector of variable names that are required to be defined
## have - put in vector output from ls(). 
## This will collect all the variables that
## are currently define
##
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

