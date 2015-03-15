

###############################################################################
## a function that takes a attempts to 'load' one object from file
##
## When load completes, the expectation is that one object is created
## This new object is then returned, allowing it to be renamed
##
## e.g.
## xx = c(1,2,3)
## save(xx,file="hello.Rda")
## rm(xx)
## zz = loadOneObject("hello.Rda")
##
## The conventional load would create an object called xx
## After this creates, the contents of hello.Rda are named as zz
## 
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
