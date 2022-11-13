library(readxl)
setwd("C:/Users/pauli/Documents/M2/R/projet/code/")
dat<-read_excel("seeds_dataset.xls")
dat


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


#Crée un jeu de données d'apprentissage et test
data<-plsda.split_sample(dat)
nrow(data$train)
nrow(data$test)



plsda.dummies<-function(y){
  # Formatage de y
  fX <- as.factor(as.vector(y))
  # modalitées du facteur
  lx <- levels(fX)
  # Matrice d'indicatrice
  dum<-sapply(lx,function(x){ifelse(fX==x,1,0)})
  return(as.matrix(dum))
}



plsda.scale<-function(X, reduce=FALSE){
  #convertir en matrice 
  X<-as.matrix(X)
  if(typeof(X)!="double"){
    stop("La matrice en entrée doit contenir que des champs numériques")
  }
  #Moyennes par colonnes
  means<-apply(X,2,mean)
  
  #centrer
  if(ncol(X)>1){
    Xnew<-t(apply(X,1,function(x){ return(x-means)}))
  }else{
    Xnew <- X-means
  }
  #reduire si reduce=TRUE
  if(reduce){
    #Variances par colonnes
    vars<-apply(X,2,var)
    #reduire par l'ecart type
    Xnew <- t(apply(Xnew,1,function(x){x/sqrt(vars)}))
    # Liste de retour
    return(as.matrix(Xnew))
  }else{
    return(as.matrix(Xnew))
  }
}






data=dat
formula = seed ~.

tol=1e-06
max.iter=500
ncomp=2

plsda.pls <- function(formula, data, ncomp =2, reduce = F, max.iter = 500, tol = 1e-06){ 
  
  #formula au bon type
  if(plyr::is.formula(formula)==F){
    stop("formula must be R formula !")
  }
  
  #Récupération des X et Y
  x <- as.matrix(model.matrix(formula, data = data)[,-1])
  y <- as.factor(model.response(model.frame(formula, data = data)))
  
  #si data est a reduire
  if(reduce == T){
    x <- plsda.scale(x, reduce=T)
  } else {
    x <- plsda.scale(x)
  }
  ydum <- plsda.dummies(y)
  
  #initialisation 
  
  comp_names <- paste0('PC', seq_len(ncomp))
  
  #matrice des poids des composantes de X
  W <- data.frame(matrix(rep(NA), nrow = ncol(x), ncol=ncomp))
  rownames(W) <- colnames(x)
  
  #matrice des coefficients des composantes de X
  P <- data.frame(matrix(nrow = ncol(x), ncol = ncomp, dimnames = list(colnames(x), comp_names)))
  
  
  #matrice des variables latentes de X
  eigTx <- vector("numeric", length = ncomp)
  names(eigTx) <- comp_names
    
  
  #vecteur des variables latentes de Y
  eigU <- vector("numeric", length = ncomp)
  names(eigU) <- comp_names
  
  #matrice des coefficients des composantes de Y
  Q <- data.frame(matrix(rep(NA), nrow = ncol(ydum), ncol = ncomp))
  rownames(Q) <- colnames(ydum)
  
  X.iter = x
  #on déroule l'algorithme NIPALS pour calculer les composantes de X et Y
  for(k in 1:ncomp){
    
    #u = premiere colonne de Yk-1
    u <- as.matrix(ydum[,1])
    
    
    w <- 1/rep(sqrt(ncol(x)), ncol(x))
    init <- vector("numeric", length = ncol(x))
    iter <- 1
    diff <- 1
    
    #on boucle jusqu'à ce que w converge
    while (diff > tol & iter <= max.iter){
    
      init <- crossprod(X.iter, u) / drop(crossprod(u))
      init <- init / drop(sqrt(crossprod(init)))
      
      #on calcule la composante t de la matrice Xk-1
      t <- X.iter %*% init / drop(crossprod(init))
      
      #calcul des poids de Yk-1
      q <- crossprod(ydum,t)/drop(crossprod(t))
      
      #calcul de la composante u de Xk-1
      u <- (ydum%*%q)/drop(crossprod(q))
      
      
      diff <- drop(sum((init - w)^2, na.rm = TRUE))
      w <- init
      iter = iter + 1
      
    }
    if (iter > max.iter){
      message(paste("Maximum number of iterations reached for comp: ", k))
    }
    
    #SVD de Y
    eigU[k] <- sum(u * u, na.rm = TRUE)
    
    #SVD de X
    eigTx[k] <- sum(t * t, na.rm = TRUE)
    Tx[,k] <- t
    
    #matrice des composantes de Y
    Q[,k] <- q
    
    #matrice des composantes "loadings" de la pls de R
    P[,k] <- init
    
    #mise à jour de la matrice des X
    X.iter <- X.iter - t %*% t(init)
    #mise à jour des Y
    ydum <- ydum - t%*%t(q)
    
    #on stocke le vecteur des poids de la composante i dans W[,i]
    W[,i] <- w
    
  }
  eigTx <- sqrt(eigTx)
  eigU <- sqrt(eigU)
  t <- scale(t, center = FALSE, scale = eigTx)
  
  train_pls <- data.frame(y, Tx)
  
  #critere a maximiser 
  #=somme des carrés des covariances entre composante et chacune des variables réponses
  R2 <- cor(ydum, Tx)^2
  
  res <- list("X"=X,
              "Y"=y,
              "Yloadings" = Q,
              "Yscores" = U,
              "Xloadings"= P,
              "Xloading.weights" = W,
              "Xscores" = Tx,
              "TrainPlsData" = train_pls,
              "R2" = R2
  )
  
  class(res)<-"PLS"
  return(res)
  
}




