## Some functions related to two-circle venn diagrams
## The functions compute a representation of the venn circles
## Plotting is up to you!




##' Create a vennobject
##'
##' Convert areas into coordinates of circle centers
##' 
##' @param sizeA numeric. Area of circle A
##' @param sizeB numeric. Area of circle B
##' @param sizeAB numeric. Area of intersection
##' @param theta numeric. Angle in radians
##' @param ... additional arguments passed on to findCircleDistance
##' 
##' @export
getVenn = function(sizeA, sizeB, sizeAB, theta=0, ...) {

  if (sizeAB>sizeA) {
    stop("sizeAB cannot be greater than sizeA\n");
  }
  if (sizeAB>sizeB) {
    stop("sizeAB cannot be greater than sizeB\n");
  }

  rA = sqrt(sizeA/pi);
  rB = sqrt(sizeB/pi);
    
  ans = list();
  ans$A = list(x=0,y=0,R=rA);
  ans$B = list(x=2,y=0,R=rB);
  class(ans)="vennobject";

  dAB = findCircleDistance(rA, rB, sizeAB, ...)
  
  temp = getCenterXvals(dAB, rA, rB);
  ans$A$x=temp[1];
  ans$B$x=temp[2];

  ## rotate the circles by a certain angle, use fact that ans has circles along x-axis
  ans2 = ans;
  for (i in 1:2) {
    ans2[[i]]$x = cos(theta)*ans[[i]]$x;
    ans2[[i]]$y = sin(theta)*ans[[i]]$x;
  }

  ans2$A$label = "A";
  ans2$B$label = "B";
  
  ## complete the remaining elements of the vennobject
  ans2 = attachLabels(ans2, cat.angle=c(0,0), cat.pos=c(2,4), cat.offset=c(1,1));
  
  return(ans2);      
}




##' Show basic information about a vennobject
##' 
##' @param x vennobject
##' @param ... Further parameters are ignored
##' 
##' @export 
print.vennobject = function(x, ...) {
    for (AB in c("A", "B")) {
        cat("Vennobject circle A:\n")
        cat("center:   ", x[[AB]]$x, ", ",x[[AB]]$y, "\n")
        cat("radius:   ", x[[AB]]$R, "\n")
        cat("label:    ", x[[AB]]$label, "\n")
        cat("labelpos: ", x[[AB]]$labelpos[1], ", ", x[[AB]]$labelpos[2], "\n")
        cat("offset:   ", x[[AB]]$offset, "\n")
        cat("\n")
    }    
}


##' Attach labels to vennobjects 
##'
##' This function saves category labels into a venn object.
##' If the object is moved, the labels will also move.
##'
##' @param vo - vennobject.
##' @param cat - character vector. Two labels for circles A and B
##' @param cat.labelpos -
##' @param cat.angle - if absolute position is not specified,
##' can specify angle (in radians) from center
##' 0 corresponds to label just above the circle.
##' @param cat.pos - a vector with four items (adjA, adjA, adjB, adjB) use this with cat.angle to
##'           tune where label appears relative to the  anchor position
##' @param cat.offset - numeric vector, distance of labels from 
##'
##' @export
attachLabels = function(vo, cat=c(vo$A$label, vo$B$label), cat.labelpos=NULL,
  cat.angle=NULL, cat.pos=c(vo$A$pos, vo$B$pos), cat.offset=c(vo$A$offset, vo$B$offset)) {

  ## label positioning in x,y plane
  if (!is.null(cat.labelpos)) {
    vo$A$labelpos = cat.labelpos[c(1,2)];
    vo$B$labelpos = cat.labelpos[c(3,4)];
  } else {
    if (!is.null(cat.angle)) {
      for (i in 1:2) {
        vo[[i]]$labelpos = c(vo[[i]]$x+(vo[[i]]$R*sin(cat.angle[i])),
                 vo[[i]]$y+(vo[[i]]$R*cos(cat.angle[i])));
      }
    }
  }

  ## label position relative to anchor coordinate
  if (is.null(cat.pos)) {
    vo$A$pos = 2;
    vo$B$pos = 4;
  } else {
    vo$A$pos = cat.pos[1];
    vo$B$pos = cat.pos[2];
  }

  ## label position relative to anchor coordinate
  if (is.null(cat.offset)) {
    vo$A$offset = 1;
    vo$B$offset = 1;    
  } else {
    vo$A$offset = cat.offset[1];
    vo$B$offset = cat.offset[2];
  }
  
  vo$A$label=cat[1];
  vo$B$label=cat[2];
  
  return (vo);
}





##' Obtain xlim and ylim for vennobject
##' 
##' @param vo - vennobject
##' 
##' @export
getVennLims.vennobject = function(vo) {  
  ans = list();
  temp = c(vo$A$x+c(vo$A$R,-vo$A$R), vo$B$x+c(vo$B$R, -vo$B$R));
  ans$xlim = range(temp);
  temp = c(vo$A$y+c(vo$A$R,-vo$A$R), vo$B$y+c(vo$B$R, -vo$B$R));
  ans$ylim = range(temp);
  return(ans);
}





##' displace the Venn diagram by a vector
##'
##' @param vo - vennobject
##' @param moveby - numeric vector of length 2.
##' 
##' @export
moveVennObject = function(vo, moveby) {

  if (length(moveby)!=2) {
    stop("moveby mu be a vector of length 2\n");
  }
  if (class(vo)!="vennobject") {
    stop("vo must be a vennobject");
  }

  return(lapply(vo,
                function(x) {
                  x$x = x$x + moveby[1];
                  x$y = x$y + moveby[2];
                  x$labelpos[1] = x$labelpos[1]+moveby[1];
                  x$labelpos[2] = x$labelpos[2]+moveby[2];
                  return(x);
                } ));    
}




###############################################################
## Helper functions

circleArea = function(rr) {
  return(pi*rr*rr);
}


##
## this function considers circles with radii rA and rB which overlap with area sizeAB
##
## the function returns the distance between the circle centers that gives rise to the required
## overlap area
##
findCircleDistance = function(rA, rB, sizeAB, mindistance=NULL, maxdistance=NULL,
  tolerance=1e-5, maxdepth=40) {

  ## check for trivial, complete overlap cases
  rmin = min(rA, rB);
  rmax = max(rA, rB);
  if (abs(sizeAB - (pi*rmin^2)) <= tolerance) {
    return(rmax-rmin);
  }

  ## the rest of the function is recursive with a maximum depth level
  if (maxdepth<=0) {
    cat("stopping due to maxdepth\n");
    return((mindistance+maxdistance)/2);
  }
  
  if (sizeAB<=0) {
    return (rA+rB);
  }

  if (is.null(mindistance)) {
    mindistance = 0;
  }
  if (is.null(maxdistance)) {
    maxdistance = rA + rB;
  }
  
  mid = (maxdistance+mindistance)/2;

  ##cat(rA,",",rB,"  find with min.d=",mindistance," max.d=",maxdistance," mid=",mid,"\n");
  
  midArea = getOverlapArea(mid, rA, rB);

  ##cat("find with min.d=",mindistance," max.d=",maxdistance," mid=",mid," midArea=",midArea,"\n");
  
  if (abs(midArea-sizeAB)<tolerance) {
    return (mid);
  }
  if (midArea<sizeAB) {
    ##cat("option A\n");
    return (findCircleDistance(rA, rB, sizeAB,
                               mindistance=mindistance, maxdistance=mid,
                               tolerance=tolerance, maxdepth=maxdepth-1));
  } else {
    ##cat("option B\n");
    return (findCircleDistance(rA, rB, sizeAB,
                               mindistance=mid, maxdistance=maxdistance,
                               tolerance=tolerance, maxdepth=maxdepth-1));
  }
  
}



getCenterXvals = function(dAB, rA, rB, disjointgap=max(rA,rB)/4) {

  if (rA+rB<dAB) {
    return(c(0,0));
  }

  if (dAB==0) {
    return(c(0,0));
  }
  if (dAB==(rA+rB)) {
    return(c(-rA-disjointgap/2, rB+disjointgap/2));
  }
  
  
  ## get distances from line of intercept to the circle centers
  aa = ((dAB^2)+(rA^2)-(rB^2))/(2*dAB);  
  bb = dAB-aa;
  
  ## translate the aa, bb into coordinates, i.e. put circleA on the left and circle B on the right
  aa = -aa;
  
  return(c(aa, bb));
}




##
## takes: distance between two circle centers, two radii
## returns: the area of overlap between the two circles
##
getOverlapArea = function(dAB, rA, rB) {

  ## check for extreme cases:
  
  ## case of complete overlap
  rmin = min(rA, rB);
  rmax = max(rA, rB);
  if (dAB+rmin <=rmax) {
    return (pi*(rmin^2));
  }
    
  temp = getCenterXvals(dAB, rA, rB);
  aa = temp[1];
  bb = temp[2];
  
  yy = sqrt(rA^2-aa^2);
  alpha = abs(acos(abs(aa)/rA));
  beta = abs(acos(abs(bb)/rB));
  
  ## calculate the overlap. Formula is a bit different depending on degree of overlap  
  if (sign(aa)*sign(bb)<0) {
    ## calcualate overlap if interesection point is between the two centers            
    overlap = ((rA^2)*alpha)+((rB^2)*beta)-(yy*(abs(aa)+abs(bb)));
  } else if (sign(aa)==-1 & sign(bb)==-1){
    ## calculate overlap if both centers are to the left
    overlap = ((rA^2)*alpha) - (yy*abs(aa)) + ((pi-beta)*(rB^2)) + (yy*abs(bb));                
  } else {
    overlap = ((rB^2)*beta) - (yy*abs(bb)) + ((pi-alpha)*(rA^2)) + (yy*abs(aa));
  }
  return(overlap);
  
}


