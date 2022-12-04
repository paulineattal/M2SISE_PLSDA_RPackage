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




plslda.nipals <- function(X, y, ncomp, max.iter, tol){ 
  
  #on ne fait pas de vérifications car la fonction est seulement appelée en interne de la fit()
  
  X.init <- as.matrix(X)
  Y.init <- as.matrix(y)
  
  #################
  #initialisations#
  #################
  
  comp_names <- paste0('PC', seq_len(ncomp))
  
  #matrice des coordonnées des composantes de X
  #/ coefficients de projection des composantes X
  #/ poids de X
  W <- data.frame(matrix(rep(NA), nrow = ncol(X.init), ncol=ncomp))
  rownames(W) <- colnames(X.init)
  names(W) <- comp_names

  #matrice des variables latentes de X 
  #/ composantes principales
  Tx <- data.frame(matrix(rep(0), nrow = nrow(X.init), ncol=ncomp))
  names(Tx) <- comp_names
  #eigTx <- vector("numeric", length = ncomp)
  
  #matrice des variables latentes de Y
  #c'est ce qui fait que la nipals est une ACP supervisée
  U <- data.frame(matrix(rep(0), nrow = nrow(Y.init), ncol=ncomp))
  names(U) <-comp_names
  
  #matrice des coefficients des composantes de Y
  #/ coefficients de projection des composantes Y
  #/ poids de Y
  Q <- data.frame(matrix(rep(NA), nrow = ncol(Y.init), ncol = ncomp))
  rownames(Q) <- colnames(Y.init)
  names(Q) <- comp_names
  
  nc.ones <- rep(1, ncol(X))
  nr.ones <- rep(1, nrow(X))
  
  X.iter = X.init #X0
  Y.iter = Y.init #Y0
  
  ################################
  #calculs des variables latentes#
  ################################
  
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
    
    
    #on boucle jusqu'à ce que wk converge
    while (diff > tol & iter <= max.iter){
      
      #wk
      w <- crossprod(X.iter, u) / drop(crossprod(u)) 
      #normer a 1
      w <- w / drop(sqrt(crossprod(w)))
      #calcul de la composante u de Xk-1
      #pk ca fait 1 le drop(crossprod(w)) ???
      t <- X.iter %*% w / drop(crossprod(w)) #tk
    
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
    
    ###################################################
    #stockage des résultats de chaque variable latente#
    ###################################################
    
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
  
  ########################################
  #qualité de la restitution des facteurs#
  ########################################
  
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
  
  #Data.frame pour l'affichage dans la fonction summary 
  quality <- rbind(Var.Explained.X[nrow(Var.Explained.X),], Var.Explained.X.Cum[nrow(Var.Explained.X.Cum),], Var.Explained.Y[nrow(Var.Explained.Y),], Var.Explained.Y.Cum[nrow(Var.Explained.Y.Cum),])
  rownames(quality) <- c("R2X", "R2Xcum", "R2y", "R2Ycum")
  quality <- t(quality)
  
  ##################################
  #stockage des résultats de sortie#
  ##################################
  
  res <- list("comp_X"= Tx,
              "poid_X" = W, 
              "comp_Y" = U, 
              "poid_Y" = Q,
              "Y.iter" = Y.iter,
              "quality" = quality
  )
  
  class(res)<-"PLSDA"
  return(res)
  
}


