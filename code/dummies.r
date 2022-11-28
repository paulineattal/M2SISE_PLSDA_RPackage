
#fonction pour creer des dummies
#a partir d'une variable de type factor
#codage disjonctif complet 
plsda.dummies<-function(y, mod=TRUE){
  
  #formatage de y
  y <- as.factor(as.vector(y))
  #nombre de modalités
  if (length(mod)!=0){
    mod <- as.factor(as.vector(mod))
    n.mod <- levels(mod)
  }else{
    n.mod <- levels(y)
  }

  # Matrice d'indicatrice 0/1
  dum<-sapply(n.mod,function(x){ifelse(y==x,1,0)})
  #renvoyer sous la forme de data frame
  return(as.data.frame(dum))
}

test_dum=plsda.dummies(pred,Y.test)
