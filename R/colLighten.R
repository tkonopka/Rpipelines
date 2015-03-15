## input a color,
## output a different color lightened/darkened by some amount
##
## col is a color e.g. #2378aa
## lighten is a number [0,whatever]
##
## function also accepts vectors of colors 
##
## Author: Tomasz Konopka
##
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



## analog of colLighten, but this darkens the color
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




## (vectorized) function to convert a value into a transparency value
## use only values in range [0,1]
## 
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




## convert colors into "#XXXXXX" format
col2hex = function(x) {
  ans = matrix(val2hex(col2rgb(x)/255), nrow=3);
  ans = apply(ans, 2, paste, collapse="");
  ans = paste("#", ans, sep="");
  names(ans) = names(x);
  return(ans);
}




## "average" rgb values of colors
##
avgcol= function(x) {
  ans = matrix(col2rgb(x)/255, nrow=3);
  ans = apply(ans, 1, mean);
  ans  = paste(val2hex(as.numeric(ans)), collapse="");
  ans = paste("#",ans, sep="");
  return(ans);
}
