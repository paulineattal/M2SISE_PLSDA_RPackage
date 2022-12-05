#' plslda.split_sample,
#' This function is used to split data into a train and a test sets
#' @param
#' formula, used to select the predictive variables
#' @param
#' data, the data to split into train and test sets
#' @param
#' train_size, is set to 0.7 by default (70%), which means 70% of the data goes to the train set
#'
#' @return
#' It returns train-test splits of inputs
#' @export
#'
#' @examples
#' data(iris)
#' formule = Species~.
#' split_sample.t1<-plslda.split_sample(formula=formule, data=iris)
#' split_sample.t2<-plslda.split_sample(formula=formule, data=iris, iris,0.5)
#' train1 = split_sample.t1$train
#' Xtrain1 = split_sample.t1$Xtrain
#' ytrain1 = split_sample.t1$ytrain
#' Xtest1 = split_sample.t1$Xtest
#' ytest1 = split_sample.t1$ytest
#'
#'

plslda.split_sample<-function(formula, data, train_size=0.7){

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
