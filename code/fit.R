#' Title
#'
#' @param formula
#' @param data
#' @param ncomp
#' @param max.iter
#' @param tol
#'
#' @return
#' @export
#'
#' @examples
#'
#' fit(Species ~ ., data = iris, ncomp = 2)
#' fit(Species ~ Sepal.Length + Petal.Length, data = iris, ncomp = 2)
#'






fit <- function(formula, data, 
                ncomp = 4, #ici on peut mettre "CV" 
                sel_var = NA, #ici on peut mettre que backward
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
  
  #if (sel_var){
    #var_rm = backward(data)
    #data = data[setdiff(colnames(data), as.vector(rm))]
  #}
  
  #Récupération des X et Y
  X <- as.matrix(model.matrix(formula, data = data)[,-1])
  X.init <- X
  y <- as.factor(model.response(model.frame(formula, data = data)))

  
  #si data est a standardiser
  if ((mean(apply(X,2,mean))>abs(1)) || (sum(sqrt(apply(X,2,var))) != ncol(X))){
    X <- plsda.scale(X)
  }
  
  #codage disjonctif de la variable cible
  ydum <- plsda.dummies(y)
  
  #if ncomp == "CV" {
    #ncomp = plsda.cv()
  #}
  
  nipals.res <- plsda.nipals(X=X, y=ydum, ncomp=ncomp , max.iter=max.iter, tol=tol)
  
  ########################LDA########################
  
  #ici on effectue la LDA pour la classification
  #on l'a fait sur nos compossntes principales Th, obtenues en sorties de la PLS
  Th <- nipals.res$comp_X
  #Th<-t(apply(as.matrix(nipals.res$comp_X),1,function(ligne){ligne %*% t(as.matrix(nipals.res$poid_X))}))
  
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
  coef_
  colnames(coef_) <- levels(y)
  intercept_ <- log(pi_k)-0.5*diag(mb_k %*% invW %*% t(mb_k))
  #revenir a toutes les var originelles 
  coef_ <- as.matrix(nipals.res$poid_X)%*%coef_
  coef_ <- diag(1/apply(X.init, 2, sd)) %*% coef_  
  coef_
  
  
  intercept_ <- as.vector(-apply(X.init, 2, mean) %*% coef_) + log(pi_k)

  res <- list("comp_X"= nipals.res$comp_X,
              "poid_X" = nipals.res$poid_X,
              "comp_Y" = nipals.res$comp_Y,
              "poid_Y" = nipals.res$poid_Y,
              "intercept_" = intercept_, 
              "coef_"=coef_,
              "y" = y)
  
  class(res)<-"PLSDA"
  return(res)
}



