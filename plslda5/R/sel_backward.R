
#fonction pour le test du retrait d'une variable numéro j
#SW : matrice de covariance intra
#ST : matrice de covariance totale
#N : nombre d'observations
#K : nombre de classes
#precLW : valeur du lambda avant le retrait
test.retrait <- function(j,SW,ST,N,K,precLW){
  #nombre de variables à cette étape
  nbVar <- ncol(SW)
  
  
  
  #lamdba
  #c'est ce lambda qui se base sur l'AD
  #en gros si les donnees sont tres grandes, qu'on donne ca en entree
  #bah les matrices SW seront enormes !!! et ca penalisera aussi la PLS du coup ! 
  #du coup cette methode backward n'est pas forcement la mieux ... 
  #du coup voir comment faire une selection de variable apres en prenant comme critere 
  #peut etre un truc sur les coeff de la pls ??? 
  lambada <- det(SW[-j,-j])/det(ST[-j,-j])
  
  
  #ddl
  ddlNum <- K - 1
  ddlDenom <- N - K - nbVar + 1
  #tatistique de test
  f <- ddlDenom / ddlNum * (lambada / precLW - 1)
  #p-value
  pvalue <- pf(f,ddlNum,ddlDenom,lower.tail=FALSE)
  #renvoyer sous forme de vecteur
  return(c(lambada,f,pvalue))
}

#fonction de selection backward
backward <- function(data){
  if (!is.data.frame(data)){
    stop("parametre data doit etre de type data.frame")
  }
  
  #alpha : risque pour piloter la sélection
  alpha=0.05
  X <- as.matrix(model.matrix(formula, data = data)[,-1])
  y <- as.factor(model.response(model.frame(formula, data = data)))
  
  n <- nrow(data) # nombre d'observations
  mod <- levels(y)
  K <- nlevels(y) # nombre de classes
  p <- ncol(X) # nombre de variables explicatives 
  n_k <- table(y) #effectifs des classes
  lstInit <- colnames(X)
  #calculs des matrices de var.covariance pour wilks 
  #matrice initiale de covariance totale
  Vb <- (n-1)/n * cov(X)
  #matrice initiale de covariance intra
  Wb <- (1.0/n)*Reduce("+",lapply(mod,function(niveau){(n_k[niveau]-1)*cov(X[y==niveau,])}))
  #lambda de Wilks initial (incluant les p variables)
  lambda_depart <- det(Wb)/det(Vb)
  
  #matrice pour récupérer les résultats à chaque étape
  mResult <- matrix(0,nrow=0,ncol=3)
  colnames(mResult) <- c('Lambda','F-Test','p-value')
  #liste des variables retirées
  lstvarDel <- c()
  
  #boucle de recherche
  repeat{
    #resultats d'une étape
    res <- t(sapply(1:ncol(Wb),test.retrait,SW=Wb,ST=Vb,N=n,K=K,precLW=lambda_depart))
    #trouver la ligne qui maximise la p-value
    id <- which.max(res[,3])
    #vérifier si on doit faire un retrait (comparer p-value à alpha), sinon on arrete
    if (res[id,3] > alpha){
      #nom de la variable à retirer
      lstvarDel <- c(lstvarDel,lstInit[id])
      #rajouter la ligne de résultats (lambda,F-Test,p-value)
      mResult <- rbind(mResult,res[id,])
      #retirer
      lstInit <- lstInit[-id]
      #si on n'a plus de variables a retirer, on arrete 
      if (length(lstInit) == 0){
        break
      } else {
        #retirer les colonnes des matrices
        Wb <- Wb[-id,-id]
        Vb <- Vb[-id,-id]
        #mise a jour du lambda
        lambda_depart <- res[id,1]
      }
    } else {
      break
    }
  }
  #renvoi la liste des variables a enlever
  return(lstvarDel)
}
