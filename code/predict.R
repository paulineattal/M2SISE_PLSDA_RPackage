



plsda.predict<-function(ObjectPLSDA,newdata,type=c("posterior","class")){
  if (class(ObjectPLSDA)!="PLSDA") {
    stop("Object's class is not PLSDA")
  }
  
  if(missing(type)){
    type="posterior"
  }
  
  #si newdata n'est pas renseigné,la prédiction est réalisée sur le jeu de données d'entraînement
  if(missing(newdata)){
    newdata<-ObjectPLSDA$X[,colnames(ObjectPLSDA$X)]
  }
  
  #seules les variables importantes pour la prédiction sont retenues
  newdata <- newdata[,colnames(ObjectPLSDA$X)]
  
  # Récupération des moyennes
  means <- ObjectPLSDA$Xmeans
  
  # On retranche les moyennes des données d'entrainement pour centrer les données tests
  X<-t(apply(newdata,1,function(x){x-means}))
  
  # Récupération des coefficients des fonctions logit
  ncx<-ncol(X)
  coef<-ObjectPLSDA$plsda.coef
  
  ##CALCUL DES PROBABILITES D'APPARTENANCE AUX CLASSES
  #On calcule les scores de Y sans la constante
  prob <- as.matrix(X) %*% coef[-(ncx+1),]
  # On rajoute la constante
  prob<-t(apply(prob,1,function(x){x+coef[ncx+1,]}))
  
  # On applique la fonction softmax pour déterminer les probabilité d'appartenance aux classes
  prob<-t(apply(prob,1,function(x){exp(x)/sum(exp(x))}))
  
  #si le type classe est sélectionné
  if(type=="class"){
    clas<-apply(prob,1,which.max) # max des prob par lignes
    predY<-sapply(clas,function(x)ObjectPLSDA$level[x]) # nom de la calsse correspondantes
    return(predY)
    
    #si le type posterior est sélectionné
  }else{
    predY <- prob
    return(predY)
  }
}