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
  X.init <- X
  y <- as.factor(model.response(model.frame(formula, data = data)))

  
  #si data est a standardiser
  if ((mean(apply(X,2,mean))>abs(1)) || (sum(sqrt(apply(X,2,var))) != ncol(X))){
    X <- plsda.scale(X)
  }
  
  ydum <- plsda.dummies(y)
  
  nipals.res <- plsda.nipals(X=X, y=ydum, ncomp = ncomp , max.iter = max.iter, tol = tol)
  
  ########################LDA########################
  Th <- nipals.res$comp_X
  
  #effectif par classe
  n_k <- table(y) #train
  #nombre d'individus
  n <- nrow(Th)
  #nombre de modalite
  K <- nlevels(y)
  #nombre de variables desc
  p <- ncol(Th)
  #proportion par classe
  pi_k <- n_k / n
  
  #calcul des moyennes conditionelles - lignes = classes
  mb_k <- as.matrix(aggregate(Th,list(y),mean)[,2:(p+1)])
  
  #calcul des matrices de covariances conditionnelles 
  V_k <- by(as.matrix(Th),list(y),cov)
  
  #matrice de covariance intra-classe W
  #calculée à partir des matrices conditionnelles V_k
  W <- 1/(n-K) * Reduce("+",lapply(levels(y),function(k){(n_k[k]-1)*V_k[[k]]}))
  
  #inverse de la matrice W
  invW <- solve(W)
  
  #calcul des coefficients des variables akj
  #pour la fonction de classement
  coef_ <- t(mb_k %*% invW)
  colnames(coef_) <- levels(y)
  #revenir a toutes les var originelles 
  coef_ = as.matrix(nipals.res$poid_X)%*%coef_
  coef_ = diag(1/apply(X.init, 2, sd)) %*% coef_ %*% diag(apply(ydum, 2, sd)) 
  
  intercept_ <- log(pi_k)-0.5*diag(mb_k %*% invW %*% t(mb_k))
  t(as.matrix(nipals.res$poid_Y))%*%as.matrix(intercept_)
  diag(1/apply(X, 2, sd)) %*% intercept_ %*% diag(apply(ydum, 2, sd))
  
  
  
  res <- list("comp_X"= nipals.res$comp_X,
              "poid_X" = nipals.res$poid_X,
              "comp_Y" = nipals.res$comp_Y,
              "poid_Y" = nipals.res$poid_Y,
              "intercept_" = intercept_, 
              "coef_"=coef_non_scale,
              "y" = y)
  
  class(res)<-"PLSDA"
  return(res)
}


###################
data<-read_excel("C:/Users/pauli/Downloads/Data_LDA_Python.xlsx")
formula<-TYPE~.

fit.plslda = fit(formula, data, ncomp=4)
fit.plslda$coef_
fit.plslda$intercept_
fit.plslda$poid_X

