##
## Some function for computing Spearman correlations
## in contrast with the base functions, these produce only the correlation rho
## without estimating the p-values. This speeds up things a little
##
## Author: Tomasz Konopka
##


#
# xranks
# yrank 
#
spearmanRhoFromRanks = function(xranks, yranks) {
  xmid = mean(xranks);
  ymid = mean(yranks);
  TA = sum((xranks-xmid)*(yranks-ymid))
  TB = sqrt(sum((xranks-xmid)^2)*sum((yranks-ymid)^2));
  return(TA/TB);
}

#' x
#' y
spearmanRho = function(x, y) {
  return(spearmanRhoFromRanks(rank(x), rank(y)));
}



