plsda.dummies<-function(y){
  # Formatage de y
  fX <- as.factor(as.vector(y))
  # modalitées du facteur
  lx <- levels(fX)
  # Matrice d'indicatrice
  dum<-sapply(lx,function(x){ifelse(fX==x,1,0)})
  return(as.matrix(dum))
}
