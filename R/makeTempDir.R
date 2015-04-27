
##' Create a temporary directory
##'
##' Creates a tempoary directory. Returns path to this new directory
##' 
##' @param basedir - location where the temporary directory will be placed
##' @param label - a prefix for the temporary directory name
##' @param id - a suffix for the temporary directory. If left NULL, a random number is used
##'
##' @export
makeTempDir = function(basedir, label, id=NULL) {

  if (!file.exists(basedir)) {
    stop("basedirectory does not exist\n");
  }
  basedir = normalizePath(basedir);
  
  if (is.null(id)) {
    ## the user wants a random id number
    ## try creating random directories until success
    foundtemp = FALSE;
    while (!foundtemp) {
      tempnumber = floor(runif(1,1,1e9));
      tempdir = paste(basedir,"/",label,".",tempnumber,sep="");
      if (!file.exists(tempdir)) {
        dir.create(tempdir);
        return(tempdir);
      }
    }
        
  } else {
    ## the user wants a specific id for the directory
    tempdir = paste(basedir,"/",label,".",id,sep="");
    if (!file.exists(tempdir)) {
      dir.create(tempdir);
    }
    return (tempdir);
  }
    
}
