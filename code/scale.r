plsda.scale<-function(X, reduce=FALSE){
  #convertir en matrice 
  X<-as.matrix(X)
  if(typeof(X)!="double"){
    stop("La matrice en entrée doit contenir que des champs numériques")
  }
  #Moyennes par colonnes
  means<-apply(X,2,mean)
  
  #centrer
  if(ncol(X)>1){
    Xnew<-t(apply(X,1,function(x){ return(x-means)}))
  }else{
    Xnew <- X-means
  }
  #reduire si reduce=TRUE
  if(reduce){
    #Variances par colonnes
    vars<-apply(X,2,var)
    #reduire par l'ecart type
    Xnew <- t(apply(Xnew,1,function(x){x/sqrt(vars)}))
    # Liste de retour
    return(as.matrix(Xnew))
  }else{
    return(as.matrix(Xnew))
  }
}