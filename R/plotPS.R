##
## Generic add-ons to start/end writing image info to a file
##
## Author: Tomasz Konopka (CeMM)



## use this to start writing into a PS file
## (can take multiple paramters, but only uses "file" and "fsize");
##
plotPSstart = function(file=NULL, fsize=c(4,4), ...) {
  if (!is.null(file)) {
    postscript(file=file, paper="special", width=fsize[1], height=fsize[2],
               horizontal=FALSE);
  }
}


## use this to start writing into a PDF file
## (analogous to plotPSstart)
##
plotPDFstart = function(file=NULL, fsize=c(4,4), useDingbats=F,  ...) {
  if (!is.null(file)) {
    pdf(file=file, paper="special", width=fsize[1], height=fsize[2], useDingbats=useDingbats, ...);
  }
}


## use this to terminate the PS
## (can take multiple paramters, but only uses "file")
##
plotPSend = function(file=NULL, ...) {
  if (!is.null(file)) {
    dev.off();
  }
}


## same as plotPSend
##
plotPDFend = function(file=NULL, ...) {
  if (!is.null(file)) {
    dev.off();
  }
}
