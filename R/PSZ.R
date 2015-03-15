##' A wrapper for paste()
##'
##' Function concatenates string without putting in additional spaces.
##' Name of function stands for Pasta Sep Zero.
##'
##' @param ... any number of objects to be concatenated into one string
##' @export
PSZ = function(...) {
   return(paste(...,sep=""));
}

