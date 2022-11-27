
#fonction pour creer des dummies
#a partir d'une variable de type factor
#codage disjonctif complet - sans modalité de référence
plsda.dummies<-function(y){
  # Formatage de y
  fX <- as.factor(as.vector(y))
  #nombre de modalités
  lx <- levels(fX)
  # Matrice d'indicatrice 0/1
  dum<-sapply(lx,function(x){ifelse(fX==x,1,0)})
  #renvoyer sous la forme de data frame
  return(as.data.frame(dum))
}
