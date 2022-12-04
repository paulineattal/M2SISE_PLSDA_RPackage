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


plslda.fit <- function(formula, data, 
                ncomp = 2, #ici on peut mettre "CV" 
                max.iter = 100,
                tol = 1e-06)
{
  
  ###########################
  #vérifications des entrées#
  ###########################
  
  #paramètres renseignés
  if ((missing(data) | missing(formula))){
    stop("Erreur : formula et data sont les deux paramètres obligatoires")
  }
    
  #paramètres formula
  if(plyr::is.formula(formula)==F){
    stop("Erreur : formula doit etre de type formule")
  }
  
  #paramètres data
  if (!is.data.frame(data)){
    stop("Erreur : data doit être un data.frame")
  }
  
  #ligne.s ou colonne.s entièrement vide.s ?
  if (any(colSums(!is.na(data)) == 0) | any(rowSums(!is.na(data)) == 0 )){
    stop("Erreur : certaines lignes ou colonnes sont entierements manquantes",
         "Retirez-les avant de relancer la fonction fit().")
  }
  
  #récupération des X et Y
  X <- as.matrix(model.matrix(formula, data = data)[,-1])
  X.init <- X
  y <- as.factor(model.response(model.frame(formula, data = data)))
  
  #type des variables X tous numériques
  nbNumeric<- sum(sapply(X,is.numeric))
  if(nbNumeric<ncol(X)){
    stop("Erreur : certaines variables ne sont pas numériques")
  }
  
  #############
  #Traitements#
  #############
  
  #lancer les traitements correspondant aux paramétrages
  
  #paramètres ncomp
  #choix du nombre idéal de composantes principales
  if(ncomp == "CV") {
    ncomp = plsda.cv()$ncomp
  }else if(!is.numeric(ncomp) || is.null(ncomp) || ncomp <= 0 || length(ncomp)>1){
    stop("Erreur : paramètres ncomp doit être un numériques ")
  }else if(ncomp > qr(X)$rank){
    ncomp <- qr(X)$rank
  }
  
  #####################
  #préparer les X et y#
  #####################
  
  #si X est a standardiser
  if ((round(mean(apply(X,2,mean))) != 0) || (sum(sqrt(apply(X,2,var))) != ncol(X))){
    X <- plsda.scale(X)
  }
  
  #codage disjonctif de la variable cible
  ydum <- plsda.dummies(y)

  ########
  #NIPALS#
  ########
  
  #Appel de la nipals pour effectuer la regression PLS
  nipals.res <- plslda.nipals(X=X, y=ydum, ncomp=ncomp , max.iter=max.iter, tol=tol)
  
  #####
  #LDA#
  #####
  
  #ici on effectue la LDA pour la classification
  #on l'a fait sur nos compossantes principales Th, obtenues en sorties de la PLS
  Th <- nipals.res$comp_X

  #effectif par classe
  n_k <- table(y) 
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
  intercept_ <- log(pi_k)-0.5*diag(mb_k %*% invW %*% t(mb_k))
  
  ######################################
  #revenir à toutes les var originelles#
  ######################################
  
  coef_ <- as.matrix(nipals.res$poid_X)%*%coef_
  coef_ <- diag(1/apply(X.init, 2, sd)) %*% coef_  
  intercept_ <- as.vector(-apply(X.init, 2, mean) %*% coef_) #TODO corriger ce calcul... 
  
  #mettre dans un data.frame les coef et constant pour le print
  coef <- data.frame(Attributes = colnames(X),coef_)
  cte <- data.frame(Attributes = "constant",t(intercept_))
  colnames(cte)[1:K+1] <- levels(y)
  coef_cte <- rbind(coef,cte)
  
  ##################################
  #stockage des résultats de sortie#
  ##################################
  
  res <- list("comp_X"= nipals.res$comp_X,
              "poid_X" = nipals.res$poid_X,
              "comp_Y" = nipals.res$comp_Y,
              "poid_Y" = nipals.res$poid_Y,
              
              "quality" = nipals.res$quality,
              
              "intercept_" = intercept_, 
              "coef_"=coef_,
              "coef_cte" = coef_cte,
              
              "X.init" = X.init,
              "y" = y)
  
  class(res)<-"PLSDA"
  return(res)
}

