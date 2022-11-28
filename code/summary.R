#' Randomly Sample a DataSet
#'
#' This function randomly separate a data set into a learning sample and test sample that can be selected by the user.
#' @usage
#' plsda.split_sample(data,prop.train=0.75)
#' @param
#' data the dataset to split.
#' @param
#' prop.train proportion of the learning sample.
#' @return
#' \code{train} a subset matrice of data corresponding to the learning sample dataset.
#' \cr
#' \code{test} a subset matrice of data corresponding to the test sample dataset.
#' @examples
#' split_sample.t1<-plsda.split_sample(iris)
#' split_sample.t2<-plsda.split_sample(iris,0.5)

#surcharge de summary
summary.PLSDA <- function(object,ncomp=2){
  #vérification du nombre de composantes
  if (is.null(ncomp) || ncomp <= 0 || ncomp > length(object$vp))
  {
    ncomp = min(2,length(object$Xscores))
  }
  #affichage 
  
  
  #voir td seance 5
  
}
