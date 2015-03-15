
##
## Simple function that takes a vector of color values
## makes a bar plot showing all the colors, with their labels
## so that they can be viewed and compared
##
## Author: Tomasz Konopka
##

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

