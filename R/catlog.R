

##' Print a log message
##'
##' Print a message including a timestamp. The output will include a newline
##' character at the end.
##' 
##' @param ... any number of elements to print beside the timestamp
##' @export
catlog = function(...) {  
  cat(paste("[",Sys.time(),"]\t",paste(...,sep=""),"\n",sep=""));
}


##' Print and return a log message
##'
##' Print a message including a timestamp. Also return the content of the
##' message. The output includes a newline character at the end.
##'
##' @param ... any number of elements to print beside the timestamp
##' @export
catlogreturn = function(...) {
  tt = (paste("[",Sys.time(),"]\t",paste(...,sep=""),"\n",sep=""));
  cat(tt);
  return(tt);  
}


##' Print a vector, one item per line
##'
##' Print a vector, one item per line
##'
##' @param x a vector of elements
##' @export
catvec = function(x) {
  for (i in x) {
    cat(i,"\n");
  }
}
