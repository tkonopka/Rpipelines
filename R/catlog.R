

## print out a message to the screen
## formated with date
##
catlog = function(...) {  
  cat(paste("[",Sys.time(),"]\t",paste(...,sep=""),"\n",sep=""));
}


## same as catlog, but also return the log string
catlogreturn = function(...) {
  tt = (paste("[",Sys.time(),"]\t",paste(...,sep=""),"\n",sep=""));
  cat(tt);
  return(tt);  
}


## print elements of a vector, one item per line
catvec = function(x) {
  for (i in x) {
    cat(i,"\n");
  }
}
