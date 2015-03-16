##
## a function that will compile a latex document and 
##
## filename should be the name of the latex document
## latexmode should be either "latex" or "pdflatex"
##    Both modes will give  a pdf document at the end,
##    but use "latex" if document has eps figures (tex -> dvi -> pdf)
##    and use "pdflatex" if document has pdf figures (tex -> pdf)
##
## Author: Tomasz Konopka (CeMM)
##
##' @export
compileLatexDoc = function(filename, latexmode="latex", intern=TRUE) {

  ## go to the directory where the file is stored
  nowdir = getwd();
  filedir = dirname(filename);
  filebase = basename(filename);
  setwd(filedir);

     
  ## perform the compilation
  ## compile twice to get all references, page numbers, table of contents right
  if (latexmode=="latex") {
    
    system(paste("latex ",filename,sep=""), intern=intern);
    system(paste("latex ",filename,sep=""), intern=intern);
    
    ## get the dvi filename and make the pdf
    filename2 = substring(filename, 1, nchar(filename)-3);
    filename2 = paste(filename2, "dvi",sep="");
    
    system(paste("dvipdf ",filename2,sep=""), intern=intern);
    
  } else {
    
    ## for pdflatex, it is a one command operation
    system(paste("pdflatex ",filebase,sep=""), intern=intern);
    system(paste("pdflatex ",filebase,sep=""), intern=intern);
  }
  
  ## return to the original directory
  setwd(nowdir);    
}


