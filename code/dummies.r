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



plsda.dummies<-function(y, mod=NA){
  
  #formatage des entrées
  y <- as.factor(as.vector(y))
  mod <- as.factor(as.vector(mod))
  
  #si on a renseigné le paramètre mod
  if (!any(is.na(mod))){
    mod <- as.factor(as.vector(mod))
    n.mod <- levels(mod)
  } else {
    n.mod <- levels(y)
  }

  #matrice d'indicatrice 0/1
  dum<-sapply(n.mod,function(x){ifelse(y==x,1,0)})
  
  ########
  #sortie#
  ########
  
  #renvoyer sous la forme de data frame
  return(as.data.frame(dum))
}

