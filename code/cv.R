#' plslda.cv,
#' Cross validation for PLSLDA
#'
#' @param
#' formula : this parameter is an object of class formula
#' @param
#' data : the data to perform the cross validation on and from which to get the samples
#' @description
#' This function allows the user to apply a k-fold cross validation on the data in order to determine the optimal number of components \code{ncomp} to keep
#' the \code{ncomp} that we get from this cv function can also be used in \code{plslda.fit} if the parameter of the function is set to "CV"
#' @return
#' This function returns a list object containing :
#' \code{ncomp} the number of principal components (or latent variables) that should be used in the fit function, if not given by the user.
#' \cr
#' \code{PRESS} PRESS is a vector of PRESS values for each component.
#' \cr
#' \code{min.PRESS}the minimum value of the PRESS vector that has been calculated, giving the number of principal components to keep.
#' @export
#'
#' @examples
#' data(iris)
#' plslda.cv(Species~., data = iris)
#' plslda.cv(Species~.,data=iris, nfold = 50)


plslda.cv<-function(formula,data){
  #TODO : adapter la taille du nfold en fonction de la taille du jeu de données
  nfold=10
  
  #pas de vérifications car les paramètres d'entrées ont déja été vérifiés dans la fit()
  
  #################
  #initialisations#
  #################
  
  #récupération des X et Y
  X <- as.matrix(model.matrix(formula, data = data)[,-1])
  Y <- as.factor(model.response(model.frame(formula, data = data)))
  
  #variable de comptae des erreurs de predictions
  PRESS <- NULL
  
  #rang de la matrice X
  #au max on peut avoir rang(matrice) composantes 
  ncomp <- qr(X)$rank
  
  #tester tous les découpages de fold pour chaque "nombre de composante"
  for(j in 1:ncomp){
    #initialisation du critère d'optimisation
    press <- NULL
  
    #mélanger les X
    s<-sample(1:nrow(X),nrow(X))
    newX <- X[s,]
    newY <- Y[s]
    
    #créer les indices pour le nfolds
    folds <- cut(seq(1, nrow(X)), breaks = nfold, labels=FALSE)
    
    for(i in 1:nfold){
      
      #récuperer les indices pour le fold en cours
      indexes <- which(folds == i, arr.ind = TRUE)
      
      #on divise les données en test et entraînement
      X.train <- newX[-indexes,]
      X.test <- newX[indexes,]
      Y.train <- newY[-indexes]
      Y.test <- newY[indexes]
      train <- data.frame("Y"=Y.train, X.train)
      
      #on exécute le modèle sur les données d'appprentissage
      fit<-fit(Y~., train, ncomp = j)
      #on fait la prédiction sur X.test
      pred <- plslda.predict(fit, X.test)
      #dummies les pred
      #ajout du Y en deuxieèe paramètre de dummies 
      #complète le dummies avec une colonne a 0 si aucune pred pour une des modalité
      pred <- plslda.dummies(pred, Y)
      
      #on calcule le press pour le ième échantillon
      press[i] <- sum((as.matrix(Y.test)-as.matrix(pred))^2)
      #press[i] <- sum(sweep(as.matrix(Y.test), 1, as.matrix(pred), '-')^2)
      
    }
    PRESS[j] <-as.numeric(sum(press))
    print(PRESS)
  }
  #récuperer le ncomp sur pour lequel le press a été le plus petit
  ncomp <- which.min(PRESS)
  min.PRESS <- PRESS[ncomp]
  
  ##################################
  #stockage des résultats de sortie#
  ##################################
  
  object=list("ncomp" = ncomp,
              "PRESS" = PRESS,
              "min.PRESS" = min.PRESS)
  
  class(object)<-"CV"
  return(object)
}
