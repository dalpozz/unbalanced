#' Under-sampling
#'
#' The function removes randomly some instances from the majority (negative) class and keeps all instances in the minority (positive) class in order to obtain a more balanced dataset.
#' 
#' @param X the input variables of the unbalanced dataset.
#' @param Y the response variable of the unbalanced dataset. It must be a binary factor where the majority class is coded as 0 and the minority as 1.
#' @param perc percentage of sampling.
#' @param method method to perform under sampling ("percPos", "percUnder").
#' @param w weights used for sampling the majority class, if NULL all majority instances are sampled with equal weights
#' 
#'
#' @details It allows two ways to perform undersampling: 
#' i) by setting the percentage of positives wanted after undersampling (percPos method),
#' ii) by setting the sampling rate on the negatives, (percUnder method).
#' For percPos, "perc"has to be (N.1/N * 100) <=  perc <= 50, where N.1 is the number of positive and N the total number of instances. 
#' For percUnder, "perc"has to be (N.1/N.0 * 100) <=  perc <= 100, where N.1 is the number of positive and N.0 the number of negative instances. 
#'
#' @return The function returns a list: 
#'  \item{X}{input variables}
#'  \item{Y}{response variable}
#'  \item{id.rm}{index of instances removed}
#'
#'
#' @examples
#' library(unbalanced)
#' data(ubIonosphere)
#' 
#' n<-ncol(ubIonosphere)
#' output<-ubIonosphere$Class
#' input<-ubIonosphere[ ,-n]
#' data<-ubUnder(X=input, Y=output, perc=40, method="percPos")
#' newData<-cbind(data$X, data$Y)
#'
#' @export
ubUnder <-
function(X, Y, perc = 50, method = "percPos", w = NULL) {
  
  stopifnot(all(unique(Y) %in% c(0, 1)))
  
  N <- length(Y)
  i.1 <- which(Y == 1)
  N.1 <- length(i.1)
  i.0 <- which(Y == 0)
  N.0 <- length(i.0)
  if (N.1 >= N.0)
    stop("less 0s instances than 1s, the minority class has to be class 1")
  
  type = match.arg(method, c("percPos", "percUnder"))
  
  if (type == "percPos") {
    #perc < (N.1/N * 100)) means removing minority observations
    stopifnot(perc >= (N.1/N * 100), perc <= 50)
    N.0.sub <- floor(N.1 * (100 - perc) / perc)
  } 
  
  if (type == "percUnder") {
    # perc = N.1/N.0 * 100 is the minimum value allowed that correspond to percPos with perc = 50
    stopifnot(perc >= N.1/N.0 * 100, perc <= 100)
    N.0.sub <- floor(perc/100 * N.0)
  }
  
  # if the weights are not given, assign equal probability to all examples.
  if (is.null(w)) 
    w <- rep(1/N.0, N.0)
  
  if (N.0.sub <= N.0) 
    i.0.sub <- sample(i.0, N.0.sub, prob = w) else
      stop("subset of majoirty instances bigger than orginal set of majoirty instances")
  
  i.0.rm <- setdiff(i.0, i.0.sub)  
  Id <- c(i.0.sub, i.1)
  Id <- sort(Id)
  
  if (is.vector(X) != TRUE) 
    X <- X[Id, ] else X <- X[Id]
  Y <- Y[Id]
  
  return(list(X = X, Y = Y, id.rm = i.0.rm))
}
