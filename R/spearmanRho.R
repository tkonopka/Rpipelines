##
## Some function for computing Spearman correlations
## in contrast with the base functions, these produce only the correlation rho
## without estimating the p-values. This speeds up things a little
##
## Author: Tomasz Konopka
##


##' Compute Spearman rho from ranks
##'
##' Compute Spearman rho from ranks. Use this function when comparing
##' two vectors for which the ranks are already known. E.g. avoid recomputing
##' the ranks over and over when performing multiple comparisons.
##' Unlike in cor.test(x, y), this function calculates only the rho value
##' and does not bother with the p-value. 
##' 
##' @param xranks vector of ranks
##' @param yranks vector of ranks.
##' @export
spearmanRhoFromRanks = function(xranks, yranks) {
  xmid = mean(xranks);
  ymid = mean(yranks);
  TA = sum((xranks-xmid)*(yranks-ymid))
  TB = sqrt(sum((xranks-xmid)^2)*sum((yranks-ymid)^2));
  return(TA/TB);
}



##' Compute Spearman rho from two vectors
##'
##' Compute Spearman rho from two vectors. Unlike cor.test(), this function
##' computes only the correlation estimate and does not bother with
##' the p-value. Skipping the p-value makes this function considerably
##' faster than cor.test().
##'
##' @param x vector of numbers
##' @param y vector of numbers
##' @export
spearmanRho = function(x, y) {
  return(spearmanRhoFromRanks(rank(x), rank(y)));
}



