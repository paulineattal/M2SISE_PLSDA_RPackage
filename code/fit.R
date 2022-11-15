

fit <- function(X, ncomp, ...)
{
  if (!any(is.na(X)))
  {
    cat("no missing values in 'X' to impute \n")
    return(X)
  }
  
  ## check ncomp is high enough for reliable imputation
  if (ncomp < min(5, min(dim(X))))
    message("consider high 'ncomp' for more accurate ",
            "imputation of the missing values.")
  
  nipals.res <- plsda.nipals(X = X, ncomp = ncomp, ...)
  X.impute <- .fit.nipals(X = X, 
                             t = nipals.res$Xloadings,
                             eig = nipals.res$Xscores,
                             p = nipals.res$Xloading.weights
  )
  return(X.impute)
}


.reconstitute.matrix <- function(t, eig, p)
{
  t %*% diag(eig) %*% t(p)
}


.fit.nipals <- function(X, t, eig, p)
{
  X.hat <- .reconstitute.matrix(t = t, eig = eig, p = p)
  X[is.na(X)] <- X.hat[is.na(X)]
  return(X)
}



