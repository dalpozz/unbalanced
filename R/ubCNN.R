#' Condensed Nearest Neighbor
#'
#' Condensed Nearest Neighbor selects the subset of instances that are able to correctly classifing the original datasets using a one-nearest neighbor rule.
#'
#' @param X the input variables of the unbalanced dataset.
#' @param Y the response variable of the unbalanced dataset. It must be a binary factor where the majority class is coded as 0 and the minority as 1.
#' @param k the number of neighbours to use.
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
#' @references P. E. Hart. The condensed nearest neighbor rule. IEEE Transactions on Informa- tion Theory, 1968.
#'
#' @examples
#' library(unbalanced)
#' data(ubIonosphere)
#' 
#' n<-ncol(ubIonosphere)
#' output<-ubIonosphere$Class
#' input<-ubIonosphere[ ,-n]
#' data<-ubCNN(X=input, Y= output)
#' newData<-cbind(data$X, data$Y)
#'
#' @export
ubCNN <- function(X,Y,k=1,verbose=T){
  #require(FNN)
  
  #only numeric features are allowed
  is.not.num<-which(sapply(X,is.numeric)==FALSE)
  if(length(is.not.num)>0)
    stop("only numeric features are allowed to compute nearest neighbors")
  
  S.X<-X
  S.Y<-Y
  i.1<-which(Y==1)
  i.0<-which(Y==0)
  N.1<-length(i.1)
  N.0<-length(i.0)
  if(N.1==0 | N.0==0) {
    if(verbose) cat("All instances of the same class \n")
    return(list(X=X,Y=Y))
  }
  
  #initially C contains all 1s from S and one random 0 obs
  id.C<-c(i.1,sample(i.0,1))	
  C.X<-X[id.C,]
  C.Y<-Y[id.C]
  #use C to to build a 1-NN and classify all obs in S
  Y.knn<-FNN::knn(C.X,S.X,C.Y,k)
  levels(Y.knn) <- c(0, 1)
  levels(S.Y) <- c(0, 1)
  
  #move missclassified obs into C
  id.miss<-which(S.Y!=Y.knn)
  id.C<-c(id.C,id.miss)
  # id.C<-sample(id.C)
  X<-X[id.C, ]
  Y<-Y[id.C]
  #now C is consistent with S
  

  return(list(X=X,Y=Y))
}
