
#fonction pour creer des dummies
#a partir d'une variable de type factor
#codage disjonctif complet 
#le parametre mod sert a donner les modalités references a dummies 

plsda.dummies<-function(y, mod=NA){
  
  #formatage de y
  y <- as.factor(as.vector(y))
  
  #si on a renseigné le parametre mod
  if (!is.na(mod)){
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
