setwd("C:/Users/pauli/Documents/M2/R/projet/code/PLSDA_R_Package/")

source("code/nipals.r")
source("code/scale.r")
source("code/predict.r")

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
  
  XYloadings <- cbind(nipals.res$Xloadings,as.data.frame(y))
  
  ###faire les plots ici 
  
  
  return(nipals.res)
}



data<-data(iris)
formula<-Species~.

data<-read_excel("seeds_dataset.xls")
formula <- seed~.


test = fit(formula, data)
test$Coeffs
X <- as.matrix(model.matrix(formula, data = data)[,-1])
proute = plsda.predict(test, X)
y <- as.factor(model.response(model.frame(formula, data = data)))
rescol<-cbind(test$Xloadings,as.data.frame(y))



