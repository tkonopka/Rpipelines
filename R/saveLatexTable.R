##' Save a matrix as latex table
##'
##' Save a data frame as a latex table. Convert object into a matrix
##' and write it into two files. One has suffix "header.txt" and contains
##' the column names. The second has suffix "body.txt" and contains the 
##' contents of the table.
##' @export
saveLatexTable = function(Data, file=NULL, hlineevery=5, checkSpecialChars=F,
  eol=" \\\\\n", quote=F, sep=" & ", col.names=F, row.names=F, ...) {

  if (is.null(file)) {
    stop("saveLatexTable: must specify file path");
  }
  
  Data = as.matrix(Data);    
  DataRows = nrow(Data);
  DataColumns = ncol(Data);

  numfull = floor(DataRows/hlineevery);
  if (numfull == DataRows/hlineevery) {
    numfull = numfull-1;
  }    
  
  if (checkSpecialChars) {
    Data = avoidLatexSpecialChar(Data);
  }
  
  ## prepare two output files
  bodyoutput = PSZ(file,".body.txt");
  headeroutput = PSZ(file,".header.txt");
  
  toappend=FALSE;
  
  
  ## write the repeated elements by hlineevery
  if (numfull>0) {
    for (i in 1:numfull) {
      temp = Data[(1+((i-1)*hlineevery)):(i*hlineevery),];
      write.table(temp,file=bodyoutput,
                  sep=sep,row.names=row.names,col.names=col.names,
                  quote=quote,eol=eol,append=toappend);
      toappend=TRUE;
      write("\\hline", file=bodyoutput, append=T);
    }
  }
  ## write the remainder of the data
  temp = matrix(Data[(1+(numfull*hlineevery)):DataRows,],ncol=ncol(Data));
  write.table(temp,file=bodyoutput,
              sep=sep,row.names=row.names,col.names=col.names,
              quote=quote,eol=eol,append=toappend);
  
  ## write just the headers
  headervector = matrix(colnames(Data),nrow=1);
  if (row.names) {
    headervector= matrix(c("",headervector),nrow=1);
  }  
  write.table(headervector,file=headeroutput,
        sep=sep, eol=eol, row.names=row.names, col.names=F, quote=quote)
  
}





## takes an object, vector, or matrix as input
## if the object is made up characters, latex special characters are edited
avoidLatexSpecialChar = function(Data) {

   badchars = c("_","%","&","#");

   if (class(Data)=="logical" || class(Data)=="factor") {
       ## cat("found logical or factor: ",Data,"\n");
       Data = avoidLatexSpecialChar(as.character(Data));
   } else if (class(Data)=="character") {
       if (length(Data)==1) {
           ## cat("Data is ",Data);

           if (is.na(Data)) {
               return("NA");
           }

           Data=as.character(Data);
           ## cat(" -> ",Data,"\n");
           ## this is the main body of the procedure
           DataLen=nchar(Data);
           nowpos = 1;
           while (nowpos<=DataLen) {
               ##cat("looking at Data ",Data," and nowpos ",nowpos," ");
               ##cat(" is ",substr(Data,nowpos,nowpos),"\n");
               if (nowpos==1) {
                   for (badchar in badchars) {
                       if (substr(Data,nowpos,nowpos)==badchar) {
                           Data = PSZ("\\",badchar,substr(Data,nowpos+1,DataLen));
                           DataLen = DataLen+1;
                       }
                   }
               } else {
                   for (badchar in badchars) {
                       if (substr(Data,nowpos,nowpos)==badchar &&
                           substr(Data,nowpos-1,nowpos-1)!="\\") {
                           ## replace the bad character with an escaped version
                           Data = PSZ(substr(Data,1,nowpos-1),"\\",badchar,substr(Data,nowpos+1,DataLen));
                           DataLen = DataLen+1;
                       }
                   }
               }
	       nowpos = nowpos+1;
           }

       } else if (length(Data)==0) {
           ## Don't do anything
           Data = "";
       } else {
           ## apply AvoidLatexSpecialChar to each element in the vector of strings
           for (i in 1:length(Data)) {
               ## cat("in character length loop, sending: ",Data[i],"\n");
	       Data[i]=avoidLatexSpecialChar(Data[i]);
           }
       }

   } else if (class(Data)=="matrix" | class(Data)=="data.frame") {
       ## cat("found matrix or dataframe: \n");
       if (ncol(Data)>0 & nrow(Data)>0) {
           for (i in 1:nrow(Data)) {
               for (j in 1:ncol(Data)) {
                   Data[i,j]=avoidLatexSpecialChar(Data[i,j]);
               }
	   }
       }
   } else if (class(Data)=="list") {
       ## cat("found list.\n");
       if (length(Data)>0) {
           for (i in 1:length(Data)) {
               Data[[i]]=avoidLatexSpecialChar(Data[[i]]);
           }
       }
   } else if (is.na(Data)) {
       ## cat("found NA\n");
       Data = "NA";
   }

   return(Data);
}
