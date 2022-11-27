
plsda.predict<-function(object, newdata){
  if (class(object)!="PLSDA") {
    stop("Object's class is not PLSDA")
  }
  
  if (ncol(newdata) != (nrow(object$coef_))) {
    stop("X must have the same number of columns than model")
  }
  
  scores_ <- as.matrix(newdata) %*% object$coef_
  #rajouter la constante ak0
  scores_ <- t(apply(scores_,1,function(ligne){ligne + object$intercept_}))
  
  ####softmax ou juste max ??? 
  # SoftMax
  #scores_ <- t(apply(scores_, 1, function(x) { exp(x) / sum(exp(x)) }))
  #pred_ <- colnames(scores_)[apply(scores_, 1, which.max)]
  #max 
  pred_ <- apply(scores_,1, function(ligne){levels(object$y)[which.max(ligne)]})
  
  class(pred_)<-"PLSDA"
  return(pred_)
}

XT<-read_excel("C:/Users/pauli/Downloads/Data_LDA_Python.xlsx", sheet="DATA_PREDICT")



pred=plsda.predict(fit.plslda, XT)
pred
yT

