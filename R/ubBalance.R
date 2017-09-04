#' Balance wrapper
#'
#' The function implements several techniques to re-balance or remove noisy instances in unbalanced datasets.
#'
#' @param X the input variables of the unbalanced dataset.
#' @param Y the response variable of the unbalanced dataset.
#' @param type the balancing technique to use (ubOver, ubUnder, ubSMOTE, ubOSS, ubCNN, ubENN, ubNCL, ubTomek).
#' @param positive the majority class of the response variable.
#' @param percOver parameter used in ubSMOTE
#' @param percUnder parameter used in ubSMOTE
#' @param k parameter used in ubOver, ubSMOTE, ubCNN, ubENN, ubNCL}
#' @param perc parameter used in ubUnder
#' @param method parameter used in ubUnder
#' @param w parameter used in ubUnder
#' @param verbose print extra information (TRUE/FALSE)
#'
#'
#' @details The argument type can take the following values: "ubOver" (over-sampling), "ubUnder" (under-sampling), "ubSMOTE" (SMOTE), "ubOSS" (One Side Selection), "ubCNN" (Condensed Nearest Neighbor), "ubENN" (Edited Nearest Neighbor), "ubNCL" (Neighborhood Cleaning Rule), "ubTomek" (Tomek Link).
#'
#'
#' @return The function returns a list: 
#'  \item{X}{input variables}
#'  \item{Y}{response variable}
#'  \item{id.rm}{index of instances removed if availble in the technique selected}
#'
#'
#' @seealso \code{\link{ubRacing}}, \code{\link{ubOver}}, \code{\link{ubUnder}}, \code{\link{ubSMOTE}}, \code{\link{ubOSS}}, \code{\link{ubCNN}}, \code{\link{ubENN}}, \code{\link{ubNCL}}, \code{\link{ubTomek}}
#'
#' @references Dal Pozzolo, Andrea, et al. "Racing for unbalanced methods selection." Intelligent Data Engineering and Automated Learning - IDEAL 2013. Springer Berlin Heidelberg, 2013. 24-31.
#'
#' @examples
#' library(unbalanced)
#' data(ubIonosphere)
#' 
#' n<-ncol(ubIonosphere)
#' output<-ubIonosphere$Class
#' input<-ubIonosphere[ ,-n]
#' 
#' # balance the dataset using SMOTE
#' data<-ubBalance(X= input, Y=output, type="ubSMOTE", percOver=300, percUnder=150, verbose=TRUE)
#' balancedData<-cbind(data$X,data$Y)
#'
#' @export
ubBalance <-
function(X, Y, type = "ubSMOTE", positive = 1, 
                      percOver = 200, percUnder = 200, k = 5, 
                      perc = 50, method = "percPos", w = NULL, 
                      verbose = FALSE) {
  
  if (any(is.na(Y))) 
    stop("Y has NAs")
  
  if (!is.factor(Y)) 
    stop("Y must be a factor")
  
  lev <- levels(Y)
  if (length(lev) != 2) 
    stop("Y must be a binary factor variable")
  
  # transform the output in the range {0, 1}
  Y <- factor(Y == positive, levels = c(FALSE, TRUE), labels = c(0, 1))
  
  
  if (length(type) > 1) 
    stop("balance type does not support multiple selection")
  
  N.0 <- length(which(Y == 0))
  N.1 <- length(which(Y == 1))
  if (N.0 == 0) {
    cat("Warning: No negative instances, skip balance \n")
    return(list(X = X, Y = Y))
  }
  
  if (N.1 == 0) {
    cat("Warning: No positive instances, skip balance \n")
    return(list(X = X, Y = Y))
  }
  
  if (N.0 == N.1) {
    cat("Warning: equal number of positives and negatives, skip balance \n")
    return(list(X = X, Y = Y))
  }
  
  if (N.0 < N.1) 
    stop(positive, " class is not the minority class")
  
  data <- NULL
  
  if (type == "ubOver")
    data <- ubOver(X, Y, k, verbose)
  
  if (type == "ubUnder")
    data <- ubUnder(X, Y, perc, method, w)
  
  if (type == "ubSMOTE")
    data <- ubSMOTE(X, Y, percOver, k, percUnder, verbose)
  
  if (type == "ubOSS")
    data <- ubOSS(X, Y, verbose)
  
  if (type == "ubCNN")
    data <- ubCNN(X, Y, k, verbose)
  
  if (type == "ubENN")
    data <- ubENN(X, Y, k, verbose)
  
  if (type == "ubNCL")
    data <- ubNCL(X, Y, k, verbose)
  
  if (type == "ubTomek")
    data <- ubTomek(X, Y, verbose)
  
  if (is.null(data)) 
    stop("technique", type, " not supported")
  
  X <- data$X
  Y <- data$Y
  id.rm <- data$id.rm
  if (is.null(id.rm)) 
    id.rm <- NA
  
  N <- length(Y)
  
  # Id <- sample(1:N)
  # if (!is.vector(X)) 
  # X = X[Id, ] 
  # else {
  # # is.vector
  # X = X[Id]
  # if (any(is.na(X))) 
  # cat("WARNINGS: vector has NAs \n")
  # if (all(X == X[1])) 
  # cat("WARNINGS: constant vector after", type, "\n")
  # }
  # Y = Y[Id]
  
  if (verbose) {
    cat("Proportion of positives after", type, ":", 
        round(length(which(Y == 1))/N * 100, digits = 2), "% of", N, "observations \n")
  }
  
  #transform the outout with the original labels
  Y <- factor(Y == 1, levels = c(FALSE, TRUE), labels = lev)
  
  return(list(X = X, Y = Y, id.rm = id.rm))
}
