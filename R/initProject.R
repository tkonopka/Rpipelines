##
## Helper function to create a directory if not already present
checkCreateDir = function(dd, verbose=TRUE) {
  if (!file.exists(dd)) {
    if (verbose) {
      cat("Creating directory: ",dd,"\n");
    }
    dir.create(dd);
  }
  dd = normalizePath(dd);
  return(dd);
}



##' Initialize a directory structure for pipeline results
##'
##' Initialize a directory structure for pipeline results.
##'
##' @param workdir name of project/pipeline directory 
##' @param subdirs subdirectories within workdir.
##' @param version a label indicating the version of the analysis (currently unused)
##' @param verbose logical. If set TRUE, function will print message to 
##' notify when actual directories/folders are created in the file system.
##' @param forceclear logical. If set TRUE, function will erase any existing
##' files under 'workdir' and then recreate the project/pipeline directory
##' structure.
##' @export
initProject = function(workdir,
  subdirs=c("notes", "figures", "tables", "Rda", "data", "code"),
  version="", verbose=TRUE, forceclear=FALSE) {
  
  ## some subdirectories will not be allowed ("work" and "version")
  if ("work" %in% subdirs) {
    stop("sorry: subdirectory work is not permitted, conflicts with object creation\n")
  }
  if ("version" %in% subdirs) {
    stop("sorry: subdirectory version is not permitted, conflicts with object creation\n")
  }
  
  ans = list();
  
  ## perhaps start project from scratch, i.e. remove the folder if it already exists
  if (file.exists(workdir) & forceclear) {
    unlink(workdir);
  }
  
  ## make sure the project directory is created
  ans$work = checkCreateDir(workdir, verbose=verbose);
  
  ## create directories for figures, notes, tables, data objects
    for (nowsub in subdirs) {
      ans[[nowsub]] = checkCreateDir(PSZ(workdir,"/",nowsub), verbose=verbose);
    }
  ans$version = version;
  
  return(ans);  
}
