

plsda.scale<-function(X, center=TRUE, scale=TRUE ){

  X<-as.matrix(X)
  if(typeof(X)!="double"){
    stop("La matrice en entrée doit contenir que des champs numériques")
  }
  
  
  #CENTRER 
  if (is.logical(center)) {
    if (center) {
      X<-t(apply(X,1,function(x){ return(x-apply(X,2,mean))}))
    }
  }
  else if (is.numeric(center) && (length(center) == ncol(x)))
    X<-t(apply(X,1,function(x){ return(x-center)}))
  
  
  #REDUIRE 
  if (is.logical(scale)) {
    if (scale) {
      X <- t(apply(X,1,function(x){x/sqrt(apply(X,2,var))}))
    }
  }
  else if (is.numeric(scale) && length(scale) == ncol(X))
    X <- t(apply(X,1,function(x){x/scale}))
  
  
  return(as.matrix(X))
  
}



