#' Edited Nearest Neighbor
#'
#' Edited Nearest Neighbor removes any example whose class label differs from the class of at least two of its three nearest neighbors.
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
#' @references D. Wilson. Asymptotic properties of nearest neighbor rules using edited data. Systems, Man and Cybernetics, IEEE Transactions on, 408-421, 1972.
#'
#' @examples
#' library(unbalanced)
#' data(ubIonosphere)
#' 
#' n<-ncol(ubIonosphere)
#' output<-ubIonosphere$Class
#' input<-ubIonosphere[ ,-n]
#' data<-ubENN(X=input, Y= output)
#' newData<-cbind(data$X, data$Y)
#'
#' @export
ubENN <-
function(X,Y, k=3, verbose=TRUE){
  
  stopifnot(k > 0, class(verbose) == "logical", all(unique(Y) %in% c(0, 1)))
  
  #only numeric features are allowed
  if(any(sapply(X,is.numeric)==FALSE))
    stop("only numeric features are allowed to compute nearest neighbors")
  
  i.1<-which(Y==1)
  i.0<-which(Y==0)
  
  if(length(i.0)==0){
    #if there are no 0 obs then don't do anything
    if(verbose) 
      cat("Warning: No negative instances \n")
    return(list(X=X,Y=Y))
  }
  
  #removes only example from the majority class
  timeRemove<-system.time({
    out.hat <- FNN::knn(train=X,test=X[i.0,], cl=Y, k=k+1,prob=TRUE) #the 1-nn is the point itself therefore we need k+1
    proba.hat <- attr(out.hat, "prob")
    levels(out.hat) <- c(0,1)
    prob.th <- k/(k+1)
    id.miss <- which((Y[i.0]!=out.hat) & (proba.hat>=prob.th))
  })	 
  if(verbose) 
    cat("Number of instances removed from majority class with ENN:",length(id.miss),
        "\t Time needed:",round(as.numeric(timeRemove["elapsed"]),digits=2),"\n")
  
  if(length(id.miss)==0) 
    id.keep.0<-i.0
  else 
    id.keep.0<-setdiff(i.0,i.0[id.miss])
  
  Id<-c(id.keep.0,i.1)
  Id<-sort(Id)
  id.removed<-setdiff(1:nrow(X),Id)
  
  if (is.vector(X)!=TRUE) X=X[Id,]
  else  X=X[Id]
  Y=Y[Id]
  
  return(list(X=X,Y=Y,id.rm=id.removed))
}
