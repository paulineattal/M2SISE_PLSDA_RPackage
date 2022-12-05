#' plslda.predict,
#' The function used to predict in our model
#'
#' @usage
#' plslda.predict(ObjectPLSDA,newdata)
#' @param
#' plsda object, result of the fit method
#' @param
#' newdata, optional parameter : if given, it is a dataframe used to select the predictive variables
#' If not, the fitted values are used.
#' @return
#' the function returns pred_, which is a vector containing, for each individual of the matrix X, the class name of the highest probability of belonging to a the target variable Y class (modality)
#' @export
#'
#' @examples
#'
#' #' data(iris)
#' formula = Species~.
#'
#' data_split = plslda.split_sample(formula=formule, data=iris)
#'
#' object = plslda.fit(formula=formule, data=data_split$train)
#'
#' ypred = plslda.predict(object=object,newdata=data_split$Xtest)
#'


plslda.predict<-function(object, newdata){

  ###########################
  #verifications des entrées#
  ###########################

  #parametres non vides
  if ((missing(object) | missing(newdata))){
    stop("Erreur : object et newdata sont les deux parametres obligatoire de la fonction predict")
  }

  #vérif. class object
  if (class(object)!="PLSDA") {
    stop("Erreur : Object n'est pas un objet de type PLSDA")
  }

  #vérif. type data
  if (!is.data.frame(newdata)){
    newdata = as.data.frame(newdata)
  }

  #vérif. coherence modele et nouvelle données
  # if (ncol(newdata) != (nrow(object$coef_))) {
  #   stop("Erreur : newdata doit avoir le meme nombre de colonne que le modele")
  # }

  ########
  #Scores#
  ########

  scores_ <- as.matrix(newdata) %*% as.matrix(object$coef_)
  #rajouter la constante ak0
  scores_ <- t(apply(scores_,1,function(ligne){ligne + object$intercept_}))

  ###############
  #Classe finale#
  ###############

  #Choix de la modalité a retenir pour chaque nouvel individu

  #softmax ou juste max du score d'appartenance a chaque modalité pour un nouvel individu
  # SoftMax
  scores_ <- t(apply(scores_, 1, function(x) { exp(x) / sum(exp(x)) }))
  pred_ <- colnames(scores_)[apply(scores_, 1, which.max)]
  #max
  #pred_ <- apply(scores_,1, function(ligne){levels(object$y)[which.max(ligne)]})

  ########
  #sortie#
  ########

  class(pred_)<-"PLSDA"
  return(pred_)
}
