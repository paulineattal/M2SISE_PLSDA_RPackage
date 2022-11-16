



plsda.predict<-function(ObjectPLSDA,newdata,type=c("posterior","class")){
  if (class(ObjectPLSDA)!="PLSDA") {
    stop("Object's class is not PLSDA")
  }
  
  ################recuperer coeff dans nipals
  
  
  if (ncol(X) != (nrow(object$Coeffs)-1)) {
    stop("X must have the same number of columns than model")
  }
  
  # Setting data
  coeffs <- object$Coeffs
  X <- as.matrix(X)
  B <- coeffs[-1,]
  Cte <- matrix(rep(coeffs[1,], each=nrow(X)), nrow(X), ncol(B))
  
  # Prediction
  Y.hat <- X %*% B + Cte
  
  # SoftMax
  Y.hat <- t(apply(Y.hat, 1, function(x) { exp(x) / sum(exp(x)) }))
  y.hat <- colnames(Y.hat)[apply(Y.hat, 1, which.max)]
  
  result <- structure((list(
    Y.hat = Y.hat,
    y.hat = y.hat)))
  class(result) <- "plsda-pred"
  return(result)
}