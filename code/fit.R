source("code/nipals.R")

fit <- function(formula, data, ncomp, ...)
{
  #formula au bon type
  if(plyr::is.formula(formula)==F){
    stop("formula must be R formula !")
  }
  
  if (any(colSums(!is.na(data)) == 0) | any(rowSums(!is.na(data)) == 0 )){
    stop("some rows or columns are entirely missing. ",
         "Remove those before running pca.", call. = FALSE)
  }
  
  #Récupération des X et Y
  x <- as.matrix(model.matrix(formula, data = data)[,-1])
  y <- as.factor(model.response(model.frame(formula, data = data)))
  
  #si data est a standardiser
  if ((mean(apply(x,2,mean))>abs(1)) || (sum(sqrt(apply(x,2,var))) != ncol(x))){
    x <- plsda.scale(x)
  }
  
  ydum <- plsda.dummies(y)
  
  
  if (!any(is.na(X)))
  {
    cat("no missing values in 'X' to impute \n")
    return(X)
  }
  
  ## check ncomp is high enough for reliable imputation
  if (ncomp < min(5, min(dim(X))))
    message("consider high 'ncomp' for more accurate ",
            "imputation of the missing values.")
  
  nipals.res <- plsda.nipals(X=X, y=ydum, ncomp = ncomp, ...)
  X.impute <- .fit.nipals(formula = formula, 
                          X = X, 
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



