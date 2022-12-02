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
                sel_var = NA, #sel_val=TRUE executera la selection filtre forward
                max.iter = 100,
                tol = 1e-06)
{
  
  ###########################
  #verifications des entrées#
  ###########################
  
  if ((missing(data) | missing(formula))){
    stop("formula et data sont les deux parametres obligatoires")
  }
    
  #formula au bon type
  if(plyr::is.formula(formula)==F){
    stop("formula doit etre de type formule")
  }
  
  #data est un data.frame ?
  if (!is.data.frame(data)){
    stop("data doit être un data.frame")
  }
  
  #ligne.s ou colonne.s entierement vide.s ?
  if (any(colSums(!is.na(data)) == 0) | any(rowSums(!is.na(data)) == 0 )){
    stop("certaines lignes ou colonnes sont entierements manquantes",
         "Retirez-les avant de relancer la fonction fit().", call. = FALSE)
  }
  
  #Récupération des X et Y
  X <- as.matrix(model.matrix(formula, data = data)[,-1])
  X.init <- X
  y <- as.factor(model.response(model.frame(formula, data = data)))
  
  #type des variables X toutes numeriques
  nbNumeric<- sum(sapply(X,is.numeric))
  if(nbNumeric<ncol(X)){
    stop("certaines variables ne sont pas numeriques")
  }
  
  
  #lancer les traitements correspondant aux parametrages
  
  #param ncomp
  #choix du nombre idéal de composantes principales
  if(ncomp == "CV") {
    ncomp = plsda.cv()$ncomp
  }else if(!is.numeric(ncomp) || is.null(ncomp) || ncomp <= 0 || length(ncomp)>1){
    stop("parametre ncomp doit etre un numerique ")
  }else if(ncomp > qr(X)$rank){
    ncomp <- qr(X)$rank
  }
  
  #param sel_var
  #selection de variable 
  if (sel_var == TRUE && ncol(X)<1000){
    var_sel = sel.forward(data)
    X = data[var_sel]
  }
  
  #####################
  #preparer les X et y#
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
  
  #Appel de la nipals pour effectuer la regression PLS#
  nipals.res <- plsda.nipals(X=X, y=ydum, ncomp=ncomp , max.iter=max.iter, tol=tol)
  
  #####
  #LDA#
  #####
  
  #ici on effectue la LDA pour la classification
  #on l'a fait sur nos compossntes principales Th, obtenues en sorties de la PLS
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
  #revenir a toutes les var originelles#
  ######################################
  
  coef_ <- as.matrix(nipals.res$poid_X)%*%coef_
  coef_ <- diag(1/apply(X.init, 2, sd)) %*% coef_  
  intercept_ <- as.vector(-apply(X.init, 2, mean) %*% coef_) #TODO corriger ce calcul... 
  
  
  ##################################
  #stockage des resultats de sortie#
  ##################################
  
  res <- list("comp_X"= nipals.res$comp_X,
              "poid_X" = nipals.res$poid_X,
              "comp_Y" = nipals.res$comp_Y,
              "poid_Y" = nipals.res$poid_Y,
              "intercept_" = intercept_, 
              "coef_"=coef_,
              "y" = y)
  
  
  data.frame(Attributes = colnames(X),object$coef_)
  
  class(res)<-"PLSDA"
  return(res)
}



