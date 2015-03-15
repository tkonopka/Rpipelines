##' Modify a color by lightening
##'
##' Modify a color by lightening. Analogous to colDarken() in this package.
##'
##' @param col color to modify. The funciton also accepts vectors of colors.
##' @param lighten number [0, whatever] indicating by how much to lighten.
##' @export
colLighten = function(col, lighten) {

  if (length(col)<1 | length(lighten)<1) {
    return(c());
  }
  
  if (length(lighten)==1) {
    temp = col2rgb(col)/255;
    temp = 1-lighten*(1-temp);
    
    ## for large lighten, temp might have negative numbers, set those to zero
    newtemp = matrix(0,nrow=nrow(temp),ncol=ncol(temp));
    ttt = which(newtemp>temp);
    temp[ttt] = 0;
    
    return( rgb(t(temp)) );
    
  } else {
    ## lighten is a vector...
    ## expand col so that it fits the length of lighten
    while (length(col) < length(lighten)) {
      col = c(col, col);
    }
    col = col[1:length(lighten)];
    
    temp = col2rgb(col)/255;
    lighten = matrix(lighten, nrow=3, ncol=length(lighten), byrow=T);
    temp = 1-lighten*(1-temp);
    
    ## for large lighten, temp might have negative numbers, set those to zero
    newtemp = matrix(0,nrow=nrow(temp),ncol=ncol(temp));
    ttt = which(newtemp>temp);
    temp[ttt] = 0;
    
    return( rgb(t(temp)) );            
  }
  
}




##' Modify a color by darkening
##'
##' Modify a color by darkening. Analogous to colLighten() in this package.
##'
##' @param col color to modify. The funciton also accepts vectors of colors.
##' @param dark number [0, whatever] indicating by how much to lighten.
##' @export
colDarken = function(col, darken) {

  if (length(col)<1 | length(darken)<1) {
    return(c());
  }

  if (length(darken)==1) {
    temp = col2rgb(col)/255;
    temp = darken*temp;

    newtemp = matrix(1, nrow=nrow(temp), ncol=ncol(temp));
    ttt = which(newtemp<temp);
    temp[ttt] = 1;
    
    return( rgb(t(temp)) );

  } else {
    ## darken is a vector
    while (length(col)<length(darken)) {
      col = c(col, col);
    }
    col = col[1:length(darken)];

    temp = col2rgb(col)/255;
    darken = matrix(darken, nrow=3, ncol=length(darken), byrow=T);
    temp = darken*temp;

    newtemp = matrix(1, nrow=nrow(temp), ncol=ncol(temp));
    ttt = which(newtemp<temp);
    temp[ttt] = 1;
    
    return(rgb(t(temp)) );
  }

}




##' Convert a value [0,1] into a hex code
##'
##' Convert a value [0,1] into a hex code. Useful for obtaining a transparency
##' code based on a floating point number.
##'
##' @param x a number or a vector of numbers to convert into hex.
##' @export
val2hex = function(x) {
  ## convert all values into hex codes
  ans = as.character(as.hexmode(round(x*255)));
  ## but avoid returning hex codes with only one character, always prepend a 0
  shortans = nchar(ans)<2;
  if (sum(shortans)>0) {
    ans[shortans] = paste("0", ans[shortans], sep="");
  }
  return(ans);
}




##' Convert colors into hex format
##'
##' Convert colors into hex format, e.g. "red" into #ff0000.
##'
##' @param x color or vector of colors to convert
##' @export
col2hex = function(x) {
  ans = matrix(val2hex(col2rgb(x)/255), nrow=3);
  ans = apply(ans, 2, paste, collapse="");
  ans = paste("#", ans, sep="");
  names(ans) = names(x);
  return(ans);
}




##' Calculate an average from a vector of colors
##'
##' Calculate an average from a vector of colors.
##'
##' @param x vector of colors.
##' @export
avgcol= function(x) {
  ans = matrix(col2rgb(x)/255, nrow=3);
  ans = apply(ans, 1, mean);
  ans  = paste(val2hex(as.numeric(ans)), collapse="");
  ans = paste("#",ans, sep="");
  return(ans);
}
