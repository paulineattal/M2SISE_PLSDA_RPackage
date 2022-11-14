plsda.split_sample<-function(data, train_size=0.7){
  
  #parametre train_size correctement renseigné
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