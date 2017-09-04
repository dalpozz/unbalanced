#' One Side Selection
#'
#' One Side Selection is an undersampling method resulting from the application of Tomek links followed by the application of Condensed Nearest Neighbor.
#' 
#' @param X the input variables of the unbalanced dataset.
#' @param Y the response variable of the unbalanced dataset. It must be a binary factor where the majority class is coded as 0 and the minority as 1.
#' @param verbose print extra information (TRUE/FALSE)
#'
#'
#' @details In order to compute nearest neighbors, only numeric features are allowed.
#'
#'
#' @return The function returns a list: 
#'  \item{X}{input variables}
#'  \item{Y}{response variable}
#'
#' @references M. Kubat, S. Matwin, et al. Addressing the curse of imbalanced training sets: one-sided selection. In MACHINE LEARNING-INTERNATIONAL WORKSHOP THEN CONFERENCE-, pages 179-186. MORGAN KAUFMANN PUBLISHERS, INC., 1997.
#'
#' @examples
#' library(unbalanced)
#' data(ubIonosphere)
#' 
#' n<-ncol(ubIonosphere)
#' output<-ubIonosphere$Class
#' input<-ubIonosphere[ ,-n]
#' data<-ubOSS(X=input, Y=output)
#' newData<-cbind(data$X, data$Y)
#'
#' @export
ubOSS <-
function(X, Y, verbose=TRUE){
  
  stopifnot(class(verbose) == "logical", all(unique(Y) %in% c(0, 1)))
  
  #only numeric features are allowed
  if(any(sapply(X,is.numeric)==FALSE))
    stop("only numeric features are allowed to compute nearest neighbors")
  
  S.X<-X
  S.Y<-Y
  i.1<-which(Y==1)
  N.1<-length(i.1)
  i.0<-which(Y==0)
  N.0<-length(i.0)
  if(N.1==0 | N.0==0) {
    cat("all instances of the same class \n")
    return(list(X=X,Y=Y))
  }
  
  #initially C contains all 1s from S and one random 0 obs
  id.C<-c(i.1,sample(i.0,1))	
  C.X<-X[id.C, ]
  C.Y<-Y[id.C]
  #use C to to build a 1-NN and classify all obs in S
  Y.knn<-FNN::knn(C.X, S.X, C.Y, k = 1)
  levels(Y.knn) <- c(0, 1)
  
  #move missclassified obs into C
  id.miss<-which(S.Y!=Y.knn)
  id.C<-c(id.C,id.miss)
  id.C <- sort(id.C)
  #id.C<-sample(id.C)
  C.X<-X[id.C, ]
  C.Y<-Y[id.C]
  #now C is consistent with S
  
  #remove from C 0s that are tomek links
  data<-ubTomek(C.X, C.Y, verbose)
  X<-data$X
  Y<-data$Y
  
  return(list(X=X,Y=Y))
}
