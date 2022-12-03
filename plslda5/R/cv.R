#' Cross Validation for Partial Least Squares Discriminant Analysis
#'
#' This function performs a k-cross-validation in order to determine the number of components \code{ncomp}
#' to use in \code{plsda.fit} function.
#' @param
#' formula an object of class "formula" (or one that can be coerced to that class):
#' a symbolic description of the model to be fitted.
#' @param
#' nfold the number of folds used for cross-validation (k=10 by default).
#' @return
#' \code{ncomp} the number of components that must be used in plsda.fit.
#' \cr
#' \code{PRESS} a vector containing the calculated PRESS for each components.
#' \cr
#' \code{min.PRESS}the minimum value of te vector PRESS that has been calculated.
#' @examples
#' plsda.cv(Species~., data = iris)
#' plsda.cv(Species~.,data=iris, nfold = 50)


plsda.cv<-function(formula,data){
  #TODO : adapter la taille du nfold en fonction de la taille du jeu de données
  nfold=10

  #Vérification que l'entrée est bien une formule Y~X
  if(plyr::is.formula(formula)==F){
    stop("formula must be R formula !")
  }

  #Récupération des X et Y
  X <- as.matrix(model.matrix(formula, data = data)[,-1])
  Y <- as.factor(model.response(model.frame(formula, data = data)))
  PRESS <- NULL

  #rang de la matrice X
  #au max on peut avoir rang(matrice) composantes
  ncomp <- qr(X)$rank
  for(j in 1:ncomp){
    #initialisation du critere d'optimisation
    press <- NULL

    #shuffle le jeu de données
    s<-sample(1:nrow(X),nrow(X))
    newX <- X[s,]
    newY <- Y[s]

    #creer les indices pour le nfolds
    folds <- cut(seq(1, nrow(X)), breaks = nfold, labels=FALSE)

    for(i in 1:nfold){

      #recuperer les indices pour le fold en cours
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
      pred <- plsda.predict(fit, X.test)
      #dummies les pred
      #ajout du Y en deuxieme parametre de dummies
      #complete le dummies avec une colonne a 0 si aucune pred pour un modalité
      pred <- plsda.dummies(pred, Y)

      #on calcule le press pour le ième échantillon
      press[i] <- sum(sweep(as.matrix(Y.test), 1, as.matrix(pred), '-')^2))

    }
    PRESS[j] <-as.numeric(sum(press))
  }
  #recuperer le ncomp sur pour lequel le press a ete le plus petit
  ncomp <- which.min(PRESS)
  min.PRESS <- PRESS[ncomp]

  object=list("ncomp" = ncomp,
              "PRESS" = PRESS,
              "min.PRESS" = min.PRESS)

  class(object)<-"CV"
  return(object)
}
