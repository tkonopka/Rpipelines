##' Display a set of colors on screen
##'
##' Displays bars of various colors on screen. Each bar will be
##' annotated with the color name. Useful during development
##' for understanding how various colors will appear together.
##'
##' @param colors vector of colors to display on screen
##' @export
showColors = function(colors) {
  oldmar = par()$mar;
  par(mar=c(2,8,2,2));
  temp = rep(1,length(colors));
  bpout = barplot(temp,col=colors,axes=F,horiz=T);
  if (!is.null(names(colors))) {
    colorlabels = paste(names(colors),"\n",colors,sep="");
  } else {
    colorlabels = colors;
  }
  axis(2,at=bpout,labels=colorlabels,lwd=0,las=1);
  par(mar=oldmar);
}

