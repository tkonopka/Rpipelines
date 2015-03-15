

##' Load one R object from and Rda file
##'
##' Function loads an object from an Rda file. In contrast to standard
##' load(), this function returns the loaded object as opposed to put it
##' into the namespace. Thus, one can place the new object into any variable,
##' not necessarily the variable name used to save it in the first place.
##' 
##' @param file filename with Rda object to load
##' @export
loadOneObject = function(file) {

  ## These objects store the current object list
  ## They have strange names so that the loaded object is unlikely to conflict
  l1.HSFWOS826SFDK = 0;
  l2.QWUDMMAZ42OWP = 0;
  l1.HSFWOS826SFDK = ls();
  l2.QWUDMMAZ42OWP = ls();

  load(file);

  l2.QWUDMMAZ42OWP=ls();

  ## find the name of the loaded object
  whereindex = which(!(l2.QWUDMMAZ42OWP %in% l1.HSFWOS826SFDK));
  if (length(whereindex)<1) {
    stop("loadOneObject error: loading ",file," overwrote a variable\n");
  }
  if (length(whereindex)>1) {
    stop("loadOneObject error: loading ",file," creates more than one object\n");
  }

  ## return the loaded object
  temp = paste("return(",l2.QWUDMMAZ42OWP[whereindex],")",sep="");
  eval(parse(text=temp));
  
}
