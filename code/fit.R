setwd("C:/Users/pauli/Documents/M2/R/projet/code/PLSDA_R_Package/")

source("code/nipals.R")
source("code/scale.r")

fit <- function(formula, data, 
                ncomp = 2, 
                max.iter = 100,
                tol = 1e-06)
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
  X <- as.matrix(model.matrix(formula, data = data)[,-1])
  y <- as.factor(model.response(model.frame(formula, data = data)))

  
  #si data est a standardiser
  if ((mean(apply(X,2,mean))>abs(1)) || (sum(sqrt(apply(X,2,var))) != ncol(X))){
    X <- plsda.scale(X)
  }
  
  ydum <- plsda.dummies(y)
  
  
  nipals.res <- plsda.nipals(X=X, y=ydum, ncomp = ncomp , max.iter = max.iter, tol = tol)
  return(nipals.res)
}




data<-read_excel("seeds_dataset.xls")
test2 = fit(seed ~., data)
test2$Xscores
test2$Xloading.weights
test2$Xloadings


