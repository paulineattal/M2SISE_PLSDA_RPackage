#' This is the predict function for the Partial Least Square Discriminant Analysis (plsda) regression.
#'
#' @param object
#' @param X
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' plsda.predict(model, ech$test, type ="class")
#'
#'
#'


plslda.predict<-function(object, newdata){
  
  ###########################
  #verifications des entrées#
  ###########################
  
  #parametres non vides
  if ((missing(object) | missing(newdata))){
    stop("object et newdata sont les deux parametres obligatoire de la fonction predict")
  }
  
  #vérif. class object 
  if (class(object)!="PLSDA") {
    stop("Objectn'est pas un objet de type PLSDA")
  }
  
  #vérif. type data
  if (!is.data.frame(data)){
    stop("data doit être un data.frame")
  }
  
  #vérif. coherence modele et nouvelle données
  if (ncol(newdata) != (nrow(object$coef_))) {
    stop("newdata doit avoir le meme nombre de colonne que le modele")
  }
  
  ########
  #Scores#
  ########
  
  scores_ <- as.matrix(newdata) %*% object$coef_
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
  
  class(pred_)<-"PLSDA"
  return(pred_)
}



