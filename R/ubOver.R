#' Over-sampling
#'
#' The function replicates randomly some instances from the minority class in order to obtain a final dataset with the same number of instances from the two classes.
#' 
#' @param X the input variables of the unbalanced dataset.
#' @param Y the response variable of the unbalanced dataset. It must be a binary factor where the majority class is coded as 0 and the minority as 1.
#' @param k defines the sampling method.
#' @param verbose print extra information (TRUE/FALSE)
#'
#'
#' @details If K=0: sample with replacement from the minority class until we have the same number of instances in each class.
#' If K>0: sample with replacement from the minority class until we have k-times the orginal number of minority instances.
#'
#'
#' @return The function returns a list: 
#'  \item{X}{input variables}
#'  \item{Y}{response variable}
#'
#'
#' @examples
#' library(unbalanced)
#' data(ubIonosphere)
#' 
#' n<-ncol(ubIonosphere)
#' output<-ubIonosphere$Class
#' input<-ubIonosphere[ ,-n]
#' data<-ubOver(X=input, Y=output)
#' newData<-cbind(data$X, data$Y)
#'
#' @export
ubOver <-
function(X, Y, k = 0, verbose=TRUE) {
  
  
  stopifnot(k >= 0, class(verbose) == "logical", all(unique(Y) %in% c(0, 1)))
  
  i.1 <- which(Y == 1)
  N.1 <- length(i.1)
  i.0 <- which(Y == 0)
  N.0 <- length(i.0)
  max.k <- floor(N.0/N.1)
      
  if (k == 0) {
    # sample with replacement from the minority class to obtain a balanced dataset
    i.1.over <- sample(i.1, N.0, replace = TRUE)
  }
  
  if (k > 0) {
    # sample with replacement from the minority class until we have k-times the orginal number of 1s
    N.1.over <- N.1 * k
    if (N.1.over > N.0) {
      if (verbose)
        cat("Max number of times allowed to replicate minority class is", max.k, 
            "\n taking as many samples as the majority class \n")
      N.1.over <- N.0
    }
    
    i.1.over <- sample(i.1, N.1.over, replace = TRUE)
  }
  
  Id = c(i.0, i.1.over)
  Id <- sort(Id)
  
  if (is.vector(X) != TRUE) 
    X = X[Id, ] else X = X[Id]
  
  Y = Y[Id]
  
  return(list(X = X, Y = Y))
}
