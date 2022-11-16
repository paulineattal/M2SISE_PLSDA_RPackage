library(readxl)
ource("code/dummies.r")
source("code/scale.r")




plsda.nipals <- function(X, y, ncomp =2, max.iter = 500, tol = 1e-06){ 
  
  
  #initialisations 
  comp_names <- paste0('PC', seq_len(ncomp))
  
  #matrice des poids des composantes de X
  W <- data.frame(matrix(rep(NA), nrow = ncol(X), ncol=ncomp))
  rownames(W) <- colnames(X)

  #matrice des variables latentes de X
  Tx <- data.frame(matrix(rep(0), nrow = nrow(X), ncol=ncomp))
  eigTx <- vector("numeric", length = ncomp)
  names(eigTx) <- comp_names
  
  
  #matrice des coefficients des composantes de Y
  Q <- data.frame(matrix(rep(NA), nrow = ncol(y), ncol = ncomp))
  rownames(Q) <- colnames(y)
  
  
  
  nc.ones <- rep(1, ncol(X))
  nr.ones <- rep(1, nrow(X))
  is.na.X <- is.na(X)
  na.X <- any(is.na.X)
  
  X.iter = X
  #on déroule l'algorithme NIPALS pour calculer les composantes de X et Y
  for(k in 1:ncomp){
    
    #u = premiere colonne de Yk-1
    u <- as.matrix(ydum[,1])
    u[is.na(u)] <- 0
    
    w <- 1/rep(sqrt(ncol(X)), ncol(X))
    
    init <- vector("numeric", length = ncol(X))
    
    iter <- 1
    diff <- 1
    
    if (na.X)
    {
      X.aux <- X.iter
      X.aux[is.na.X] <- 0
    }
    
    #on boucle jusqu'à ce que w converge
    while (diff > tol & iter <= max.iter){
      
      if (na.X)
      {
        #calcul des poids init de X 
        init <- crossprod(X.aux, u)
        Th <- drop(u) %o% nc.ones
        Th[is.na.X] <- 0
        u.cross <- crossprod(Th)
        init <- init / diag(u.cross)
        init <- init / drop(sqrt(crossprod(init)))
        #calcul de la composante u de Xk-1
        u <- X.aux %*% init
        M <- drop(init) %o% nr.ones
        M[t(is.na.X)] <- 0
        ph.cross <- crossprod(M)
        u <- u / diag(ph.cross)
      } else {
        #calcul des poids init de X
        init <- crossprod(X.iter, u) / drop(crossprod(u))
        init <- init / drop(sqrt(crossprod(init)))
        #calcul de la composante u de Xk-1
        u <- X.iter %*% init / drop(crossprod(init))
      }
      

      t <- u
      
      #calcul des poids de Yk-1
      q <- crossprod(ydum,t)/drop(crossprod(t))
      
      
      diff <- drop(sum((init - w)^2, na.rm = TRUE))
      w <- init
      iter = iter + 1
      
    }
    if (iter > max.iter){
      message(paste("Maximum number of iterations reached for comp: ", k))
    }
    
    #SVD de X
    eigTx[k] <- sum(u * u, na.rm = TRUE)
    Tx[,k] <- u
    
    #matrice des composantes de Y
    Q[,k] <- q
  
    
    #mise à jour de la matrice des X
    X.iter <- X.iter - u %*% t(init)
    #mise à jour des Y
    y <- y - u%*%t(q)
    
    #on stocke le vecteur des poids de la composante k dans W[,k]
    W[,k] <- w
    
  }
  eigTx <- sqrt(eigTx)
  
  Tx <- plsda.scale(Tx, center = FALSE, scale = eigTx)
  
  train_pls <- data.frame(y, Tx)
  
  #critere a maximiser 
  #=somme des carrés des covariances entre composante et chacune des variables réponses
  R2 <- cor(ydum, Tx)^2
  
  res <- list("Xloadings"= Tx,
              "Xloading.weights" = W,
              "Xscores" = eigTx,
              "TrainPlsData" = train_pls,
              "R2" = R2
  )
  
  class(res)<-"PLSDA"
  return(res)
  
}




