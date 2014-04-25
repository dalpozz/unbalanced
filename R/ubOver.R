ubOver <-
function(X, Y, k = 0) {
  
  i.1 <- which(Y == 1)
  N.1 <- length(i.1)
  i.0 <- which(Y == 0)
  N.0 <- length(i.0)
  
  if (k < 0) 
    stop("k must be greater or equal to zero.")
  
  if (k == 0) {
    # sample with replacement from the minority class to obtain a balanced dataset
    i.1.over <- sample(i.1, N.0, replace = T)
  }
  
  if (k > 0) {
    # sample with replacement from the minority class until we have k-times the orginal number of 1s
    N.1.over <- N.1 * k
    if (N.1.over > N.0) 
      stop("Max number of times allowed to replicate minority class is ", floor(N.0/N.1), "\n")
    
    i.1.over <- sample(i.1, N.1.over, replace = T)
  }
  
  Id = c(i.0, i.1.over)
  Id <- sort(Id)
  
  if (is.vector(X) != TRUE) 
    X = X[Id, ] else X = X[Id]
  
  Y = Y[Id]
  
  return(list(X = X, Y = Y))
}
