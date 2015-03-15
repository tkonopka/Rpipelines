##
## Function to initialize some directories for a project
##
## Input:
##
## workdir - a base working directory (will be created if not exists)
## version - a version name (subdirectories will have version name attached)
##
## Output:
##   a list with (absolute) paths to workdir, notesdir, figsdir, tablesdir
##
##
## Author: Tomasz Konopka
##


## internal function to create a directory if not already present
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



##
##
##
initProject = function(workdir, subdirs=c("notes", "figures", "tables", "Rda", "data", "code"),
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
