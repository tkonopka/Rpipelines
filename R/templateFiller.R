
##
## object TemplateFiller and associated functions
##
## The point of this object is to hold template strings
## The templates can then be used with values in a script
##





##' @export
read.templates = function(dirname) {

  if (!file.exists(dirname)) {
    stop("Directory does not exist\n");
  }

  ## get all the file names that end with .txt
  templfiles = list.files(dirname, pattern=".txt$", recursive=T, full.names=T);

  ans = list();
  for (nowfile in templfiles) {

    ## cut out the .txt from the filename
    nowname = substring(basename(nowfile), 1, nchar(basename(nowfile))-4);

    ## read from the file
    fcon = file(nowfile, open="r");
    fconread = readLines(fcon, n=4096);
    ans[[nowfile]] = c();
    while (length(fconread)>0) {
      ans[[nowname]] = c(ans[[nowfile]], fconread);
      fconread = readLines(fcon, n=4096);
    }    
    close(fcon);
    
    ans[[nowname]] = paste(ans[[nowname]] , collapse="\n");
        
  }

  class(ans)="templates";
  
  return (ans);
}






##' @export
fill.templates = function(templs, templatename, keys = c(), values=c()) {

  if (length(keys)<1 | length(values)<1 | length(keys)<length(values)) {
    stop("keys and/or values are misspecified\n");
  }

  ## check that the templatename exists among the templates templs
  if (!(templatename %in% names(templs))) {
    stop(paste("template ",templatename," does not exist\n"));
  }

  ## replace all the keys with the corresponding values
  ans = templs[[templatename]];
  for (i in 1:length(keys)) {
    ans = gsub(keys[i], values[i], ans);
  }
  
  return (ans);
}
