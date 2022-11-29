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

plsda.split_sample<-function(data, train_size=0.7){
  
  #parametre train_size conforme
  if(train_size>1 | train_size<0){
    stop("Proportion non comprise entre 0 et 1")
  }
  if (!is.data.frame(data)){
    data <- as.data.frame(data)
  }
  
  n <- nrow(data)
  # Selection des indices des individus de l'echantillon d'apprentisage
  i_sample<-sample(1:n,trunc(n*train_size))
  # Liste de sortie
  res<-list("train"=data[i_sample,],
             "test"=data[-i_sample,])
return(res)
}

