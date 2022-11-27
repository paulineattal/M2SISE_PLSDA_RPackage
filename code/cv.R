
plsda.cv<-function(formula,data){
  nfold=10
  #Vérification que l'entrée est bien une formule Y~X
  if(plyr::is.formula(formula)==F){
    stop("formula must be R formula !")
  }
  
  #Récupération des X et Y
  X <- as.matrix(model.matrix(formula, data = data)[,-1])
  Y <- as.factor(model.response(model.frame(formula, data = data)))
  
  #rang de la matrice X
  ncomp <- qr(X)$rank
  for(j in 1:ncomp){
    press <- NULL
    
    
    s<-sample(1:nrow(X),nrow(X))
    
    folds <- cut(seq(1, nrow(X)), breaks = nfold, labels=FALSE)
    
    newX <- X[s,]
    newY <- Y[s]
    
    for(i in 1:nfold){
      
      indexes <- which(folds == i, arr.ind = TRUE)
      
      
      #on divise les données test et entraînement
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
      pred <- plsda.dummies(pred)
      
      #on calcule le press pour le ième échantillon
      press[i] <- sum((Y.test-pred)^2)
      
    }
    PRESS[j] <-as.numeric(sum(press))
  }
  
  ncomp <- which.min(PRESS)
  min.PRESS <- PRESS[ncomp]
  object=list("ncomp" = ncomp,
              "PRESS" = PRESS,
              "min.PRESS" = min.PRESS)
  
  class(object)<-"CV"
  return(object)
}
