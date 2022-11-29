#' Algorithme NIPALS (Non-Linear iterative PLS)
#'
#' @param X
#' @param y
#' @param ncomp
#' @param max.iter
#' @param tol
#'
#' @return
#' @export
#'
#' @examples



plsda.nipals <- function(X, y, ncomp=8, max.iter = 500, tol = 1e-06){ 
  
  X.init <- as.matrix(X)
  Y.init <- as.matrix(y)
  
  #initialisation
  comp_names <- paste0('PC', seq_len(ncomp))
  
  #matrice des coordonnées des composantes de X
  #/ coefficients de projection de des composantes X
  #/ poids de X
  W <- data.frame(matrix(rep(NA), nrow = ncol(X.init), ncol=ncomp))
  rownames(W) <- colnames(X.init)

  #matrice des variables latentes de X 
  #/ composantes principales
  Tx <- data.frame(matrix(rep(0), nrow = nrow(X.init), ncol=ncomp))
  names(Tx) <- comp_names
  #eigTx <- vector("numeric", length = ncomp)
  
  #matrice des variables latentes de Y
  U <- data.frame(matrix(rep(0), nrow = nrow(Y.init), ncol=ncomp))
  names(U) <-comp_names
  
  #matrice des coefficients des composantes de Y
  #/ coefficients de projection de des composantes Y
  #/ poids de Y
  Q <- data.frame(matrix(rep(NA), nrow = ncol(Y.init), ncol = ncomp))
  rownames(Q) <- colnames(Y.init)
  
  
  nc.ones <- rep(1, ncol(X))
  nr.ones <- rep(1, nrow(X))
  is.na.X <- is.na(X)
  na.X <- any(is.na.X)
  
  X.iter = X.init #X0
  Y.iter = Y.init #Y0
  
  #on déroule l'algorithme NIPALS pour calculer les composantes de X et Y
  for(k in 1:ncomp){
    
    #u = premiere colonne de Yk-1
    u <- as.matrix(Y.iter[,1])
    u[is.na(u)] <- 0
    
    #initialisations
    w.old <- 1/rep(sqrt(ncol(X)), ncol(X))
    
    w <- vector("numeric", length = ncol(X))
    
    iter <- 1
    diff <- 1
    
    if (na.X)
    {
      X.aux <- X.iter
      X.aux[is.na.X] <- 0
    }
    
    #on boucle jusqu'à ce que wk converge
    while (diff > tol & iter <= max.iter){
      
      if (na.X)
      {
        #calcul des poids w de X 
        w <- crossprod(X.aux, u)
        Th <- drop(u) %o% nc.ones
        Th[is.na.X] <- 0
        u.cross <- crossprod(Th)
        w <- w / diag(u.cross)
        w <- w / drop(sqrt(crossprod(w)))
        #calcul de la composante u de Xk-1
        u <- X.aux %*% w
        M <- drop(w) %o% nr.ones
        M[t(is.na.X)] <- 0
        ph.cross <- crossprod(M)
        u <- u / diag(ph.cross)
      } else {
        #wk
        w <- crossprod(X.iter, u) / drop(crossprod(u)) 
        #normer a 1
        w <- w / drop(sqrt(crossprod(w)))
        #calcul de la composante u de Xk-1
        #pk ca fait 1 le drop(crossprod(w)) ???
        t <- X.iter %*% w / drop(crossprod(w)) #tk
      }
    
      #calcul des poids de Yk-1
      q <- crossprod(Y.iter,t)/drop(crossprod(t)) #qk
      
      #prochiane colonne de Yk-1
      u<-Y.iter  %*% q / drop(crossprod(q)) #x valeurs pour les x modalités
      
      diff <- drop(sum((w - w.old)^2, na.rm = TRUE))
      w.old <- w
      iter = iter + 1
      
    }
    if (iter > max.iter){
      message(paste("Maximum number of iterations reached for comp: ", k))
    }
    
    #SVD de X
    #eigTx[k] <- sum(t * t, na.rm = TRUE)
    
    
    #P
    P=crossprod(X.iter,t)/drop(crossprod(t))
    #mise à jour des X
    X.iter <- X.iter - t %*% t(P)
    #mise à jour des Y
    Y.iter <- Y.iter - t%*%t(q)
    
    #stocker les resultats pour la sortie 
    #matrice des composantes de X
    Tx[,k] <- t
    #matrice des composantes de Y
    U[,k] <- u
    #matrice des poids des composante X
    W[,k] <- w
    #matrice des poids des composante Y
    Q[,k] <- q
  }
  #eigTx <- sqrt(eigTx)
  
  
  #X
  Rx <- cor(X.init,Tx)^2
  colnames(Rx) <- paste(rep("Comp",ncomp), 1:ncomp, sep=" ")
  if (ncomp == 1) {
    Var.Explained.X <- rbind(Rx,Redundancy=mean(Rx))
    Rx.cum <- as.matrix(apply(Rx,1,cumsum))
    Var.Explained.X.Cum <- rbind(Rx.cum,Redundancy=mean(Rx.cum))
  } else {
    Var.Explained.X <- rbind(Rx,Redundancy=colMeans(Rx))
    Rx.cum <- t(apply(Rx,1,cumsum))
    Var.Explained.X.Cum <- rbind(Rx.cum,Redundancy=colMeans(Rx.cum))
  }
  
  #y
  Ry <- cor(Y.init,Tx)^2
  colnames(Ry) <- paste(rep("Comp",ncomp), 1:ncomp, sep=" ")
  if (ncomp == 1) {
    Var.Explained.Y <- rbind(Ry, Redundancy=mean(Ry))
    Ry.cum <- as.matrix(apply(Ry, 1, cumsum))
    Var.Explained.Y.Cum <- rbind(Ry.cum, Redundancy=mean(Ry.cum))
  } else {
    Var.Explained.Y <- rbind(Ry, Redundancy=colMeans(Ry))
    Ry.cum <- t(apply(Ry, 1, cumsum))
    Var.Explained.Y.Cum <- rbind(Ry.cum, Redundancy=colMeans(Ry.cum))
  }
  
  Var.Explained.Y
  Ry.cum
  Var.Explained.Y.Cum
  
  Var.Explained.X
  Rx.cum
  Var.Explained.X.Cum
  ###########################################
  
  #critere a maximiser 
  #=somme des carrés des covariances entre composante et chacune des variables réponses
  #R2 <- cor(y, Tx)^2 #
  
  
  res <- list("comp_X"= Tx,
              "poid_X" = W, 
              "comp_Y" = U, 
              "poid_Y" = Q,
              "Y.iter" = Y.iter
              #"Xscores" = eigTx,
              #"R2" = R2,
              #"Coeffs"=coeffs
  )
  
  class(res)<-"PLSDA"
  return(res)
  
}


