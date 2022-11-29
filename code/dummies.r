#' Factor to Dummy Matrix
#'
#' This function transforms a vector of \code{p} factors into a matrix of \code{p} dummies variables.
#' @usage
#' plsda.dummies(X, name)
#' @param
#' X the vector of factors to transform.
#' @param
#' name a vector containing the original variables names that will be the \code{colnames} of the dummy matrix.
#' @return
#' The function returns a dummy matrix with \code{p} columns, named with the vector "\code{name}".
#' @examples
#' dummies.t1<-plsda.dummies(iris$Species)
#' dummies.t2<-plsda.dummies(iris$Species,"Species")


#fonction pour creer des dummies
#a partir d'une variable de type factor
#codage disjonctif complet 
#le parametre mod sert a donner les modalités references a dummies 

plsda.dummies<-function(y, mod=NA){
  
  #formatage de y
  y <- as.factor(as.vector(y))
  #print(any(is.na(as.vector(mod))))
  mod <- as.factor(as.vector(mod))
  #print(levels(mod))
  
  #si on a renseigné le parametre mod
  if (!any(is.na(mod))){
    #print(paste('s',class(mod)))
    mod <- as.factor(as.vector(mod))
    n.mod <- levels(mod)
  } else {
    #print(paste('t',class(mode)))
    n.mod <- levels(y)
  }

  # Matrice d'indicatrice 0/1
  dum<-sapply(n.mod,function(x){ifelse(y==x,1,0)})
  #renvoyer sous la forme de data frame
  return(as.data.frame(dum))
}

