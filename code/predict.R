plsda.predict<-function(object,newdata,...){
  if (class(object)!="PLSDA") {
    stop("Object's class is not PLSDA")
  }
  
  if (ncol(newdata) != (nrow(object$coef_))) {
    stop("X must have the same number of columns than model")
  }
  
  ####softmax ou juste max ??? 
  
  # SoftMax
  #scores_ <- t(apply(scores_, 1, function(x) { exp(x) / sum(exp(x)) }))
  #y.hat <- colnames(scores_)[apply(scores_, 1, which.max)]
  
  #pred_ <- structure((list(
    #Y.hat = scores_,
    #y.hat = y.hat)))
  
  scores_ <- as.matrix(newdata) %*% object$coef_
  #rajouter la constante ak0
  scores_ <- t(apply(scores_,1,function(ligne){ligne + object$intercept_}))
  #affichage des premières lignes
  
  pred_ <- apply(scores_,1, function(ligne){levels(object$y)[which.max(ligne)]})
  
  class(pred_)<-"PLSDA"
  return(pred_)
}

XT<-read_excel("C:/Users/pauli/Downloads/Data_LDA_Python.xlsx", sheet="DATA_PREDICT")


XT <- as.matrix(model.matrix(Species~., data = split$test)[,-1])
yT <- as.factor(model.response(model.frame(Species~., data = split$test)))


pred=plsda.predict(fit.plslda, XT)
pred
yT

