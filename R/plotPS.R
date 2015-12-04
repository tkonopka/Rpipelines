##
## This file contains a number of functions related to writing an image
##  to a file
##



##' Start plotting into a PS device
##'
##' Start plotting into a PS device.
##'
##' @param file filename where to save the image. When this is not
##' specified (left NULL), the function completes without doing anything.
##' @param fsize vector with two elements giving (width, height)
##' of desired image
##' @param ... additional arguments are ignored
##' 
##' @export
plotPSstart = function(file=NULL, fsize=c(4,4), ...) {
  if (!is.null(file)) {
    postscript(file=file, paper="special", width=fsize[1], height=fsize[2],
               horizontal=FALSE);
  }
}



##' Start plotting into a PDF device
##'
##' Start plotting into a PDF device.
##'
##' @param file filename where to save the image. When this is not
##' specified (left NULL), the function completes withouth doing anything.
##' @param fsize vector with two elements giving (width, height)
##' of desired image
##' @param useDingbats logical, determines whether to use dingbats font
##' @param ... additional arguments passed to pdf()
##' 
##' @export
plotPDFstart = function(file=NULL, fsize=c(4,4), useDingbats=FALSE,  ...) {
  if (!is.null(file)) {
    pdf(file=file, paper="special", width=fsize[1], height=fsize[2],
        useDingbats=useDingbats, ...);
  }
}


##' Terminate writing into a PS device.
##'
##' Terminate writing into a PS device.
##'
##' @param file filename. When NULL is not specified (left NULL),
##' the function completes without doing anything. When filename is
##' not NULL, this function will force a call to dev.off() and complete
##' writing to a graphics device.
##' @param ... additional arguments are ignored
##' 
##' @export
plotPSend = function(file=NULL, ...) {
  if (!is.null(file)) {
    dev.off();
  }
}


##' Terminate writing into a PDF device.
##'
##' Terminate writing into a PDF device.
##'
##' @param file filename. When NULL is not specified (left NULL),
##' the function completes without doing anything. When filename is
##' not NULL, this function will force a call to dev.off() and complete
##' writing to a graphics device.
##' @param ... additional arguments are ignored
##' 
##' @export
plotPDFend = function(file=NULL, ...) {
  if (!is.null(file)) {
    dev.off();
  }
}
