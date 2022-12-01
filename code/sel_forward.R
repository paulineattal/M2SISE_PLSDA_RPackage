


sel.forward<-function(formula, data, slentry = 0.01, verbose=FALSE){
  
  ###########################
  #verifications des entrées#
  ###########################
  
  #formula au bon type
  if(plyr::is.formula(formula)==F){
    stop("formula doit etre de type formule")
  }
  
  #data est un data.frame ?
  if (!is.data.frame(data)){
    stop("data doit être un data.frame")
  }
  
  X <- as.matrix(model.matrix(formula, data = data)[,-1])
  y <- as.factor(model.response(model.frame(formula, data = data)))
  
  nbNumeric<- sum(sapply(X,is.numeric))
  if(nbNumeric<ncol(X)){
    stop("certaines variables ne sont pas numeriques")
  }
  
  #######################
  #calculs préparatoires#
  #######################
  
  #nombre d'obs.
  n <- nrow(X) #;print(n)
  #nombre d'explicatives
  p <- ncol(X) #;print(p)
  #nombre de modalités de la cible, y est forcément un factor
  K <- nlevels(y) #; print(K)
  #modalités
  K_values <- levels(y) #;print(K_values)
  #Var.covar conditionnelles
  lst_Vk <- lapply(K_values,function(k){m_k <- as.matrix(X[y==k,]);(nrow(m_k)-1)*cov(m_k)})
  #matrice W - cov. intra-classes -- biaisée
  W <- Reduce("+",lst_Vk)/(n) #;print(W)
  #matrice V - cov. totale
  V <- (n-1)/n*cov(X) #
  
  #################
  #début recherche#
  #################
  curVars <- c() #variables actuellement sélectionnées
  candidatsVars <- colnames(X) #variables candidates
  curLambda <- 1.0 #pas de var. sél., lambda = 1
  q <- 0 #nb. de var. sélectionnées à l'étape courante
  #boucler pour rechercher
  
  while (TRUE){
    #si pas de variables candidates - on sort
    if (length(candidatsVars)==0){
      print("Plus de variables candidates dispo.")
      print("Fin du processus")
      cat("\n")
      break
    }
    cat(paste("Etape :",(q+1)),"\n")
    #préparer la structure pour récupérer les résultats
    curRes <- matrix(0,nrow=length(candidatsVars),ncol=3)
    rownames(curRes) <- candidatsVars
    colnames(curRes) <- c("Lambda","F","p-value")
    #pour chaque variable candidate
    for (v in candidatsVars){
      #ajouter la variable candidate à la liste courante
      tmpVars <- c(curVars,v)
      #former les matrices intermédiaires
      tmpW <- as.matrix(W[tmpVars,tmpVars]) #; print(tmpW)
      tmpV <- as.matrix(V[tmpVars,tmpVars]) #; print(tmpV)
      #calculer le lambda
      tmpLambda <- det(tmpW)/det(tmpV)
      #stat. de test
      tmpF <- (n-K-q)/(K-1)*(curLambda/tmpLambda-1)
      #p-value
      tmpPValue <- pf(tmpF,K-1,n-K-q,lower.tail=FALSE)
      #stockage
      curRes[v,] <- c(tmpLambda,tmpF,tmpPValue)
    }
    #trier le tableau des résultats selon F décroissant, si + d'une variable
    if (nrow(curRes) > 1){curRes <- curRes[order(curRes[,"F"],decreasing=TRUE),]}
    #affichage
    print(curRes)
    #récupérer la meilleure variable (la 1ere puisque triée)... si significative 
    if (curRes[1,"p-value"] < slentry){
      #nom de la variable sélectionnée
      theBest <- rownames(curRes)[1]
      print(paste("==> Variable introduite dans le modèle :",theBest))
      #ajouter la variable dans le pool des sélectionnées
      curVars <- c(curVars,theBest)
      #supprimer cette variable de la sélection
      candidatsVars <- candidatsVars[candidatsVars != theBest]
      #cat("Reste à tester :",candidatsVars,"\n")
      cat("\n")
      #màj du nb de var. dans le pool des sélectionnées
      q <- q + 1
      #màj de lambda
      curLambda <- curRes[1,"Lambda"]
    } else {
      print("Aucune variable ne répond au critère de sélection")
      print("Fin du processus")
      #this is the end....
      break
    }
  }
  
  return(curVars)
}


