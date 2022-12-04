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

plsda.split_sample<-function(formula, data, train_size=0.7){
  
  ###########################
  #vérifications des entrées#
  ###########################
  
  #paramètre train_size
  if(train_size>1 | train_size<0){
    stop("Erreur : proportion non comprise entre 0 et 1")
  }
  
  #paramètre data
  if (!is.data.frame(data)){
    data <- as.data.frame(data)
  }
  
  #paramètre formula
  if(plyr::is.formula(formula)==F){
    stop("Erreur : formula doit être de type formula")
  }
  
  #############################
  #séparation du jeu de donnée#
  #############################
  
  n <- nrow(data)
  #Sélection des indices des individus de l'échantillon d'apprentisage
  i_sample<-sample(1:n,trunc(n*train_size))
  #Liste de sortie
  train = data[i_sample,]
  test = data[-i_sample,]
  
  Xtrain <- as.matrix(model.matrix(formula, data = train)[,-1])
  Xtest <- as.matrix(model.matrix(formula, data = test)[,-1])
  
  ytrain <- as.factor(model.response(model.frame(formula, data = train)))
  ytest <- as.factor(model.response(model.frame(formula, data = test)))
  
  ##################################
  #stockage des résultats de sortie#
  ##################################
  
  res<-list("Xtrain"=Xtrain,
            "ytrain"=ytrain,
            "Xtest"=Xtest,
            "ytest"=ytest,
            "train" = train
            )
  
  return(res)
}

