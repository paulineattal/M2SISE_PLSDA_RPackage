#' sel.forward,
#' The function used for variables selection, following the forward method
#'
#' @usage
#' sel.forward(formula, data, slentry = 0.01, verbose=FALSE)
#' @param
#' formula, used to select the predictive variables
#' @param
#' data, dataset with many numeric variables
#' @param
#' slentry, if the p value of the F test stat is lower than slentry, we stop adding variables
#' @param
#' verbose, is set to FALSE by default. If set to TRUE, it prints the intermediate tables at each step
#'
#' @details
#' This approach carries out a selection of variables before applying the model. It adds variables one by one, and
#' uses the Wilks lambda which helps determine if a variable contributes significantly through its total dispersion.
#' which means, in each forward step, the one variable that is added is the ones that gives the single best improvement to your model.
#' The idea is to keep only the variables that contribute significantly to the distance between the barycentres (class centers).
#'
#' @return
#' It returns a dataframe with individuals and only the selected variables
#' @export
#'
#' @examples
#' data(iris)
#' formule = Species~
#' sel <- sel.forward(formula=formule, data=iris, slentry = 0.01, verbose=FALSE)
#'
#'


sel.forward<-function(formula, data, slentry = 0.01, verbose=FALSE){

  ###########################
  #vérifications des entrées#
  ###########################

  #paramètre verbose
  if(!is.logical(verbose)){
    stop("Erreur : parametre verbose mal rensigné, il doit etre de type logical")
  }

  #paramètre formula
  if(plyr::is.formula(formula)==F){
    stop("Erreur : formula doit etre de type formula")
  }

  #paramètre data
  if (!is.data.frame(data)){
    stop("Erreur : data doit être un data.frame")
  }

  X <- as.matrix(model.matrix(formula, data = data)[,-1])
  y <- as.factor(model.response(model.frame(formula, data = data)))

  nbNumeric<- sum(sapply(X,is.numeric))
  if(nbNumeric<ncol(X)){
    stop("Erreur : certaines variables ne sont pas numeriques")
  }

  #######################
  #calculs préparatoires#
  #######################

  #nombre d'obs.
  n <- nrow(X)
  #nombre d'explicatives
  p <- ncol(X)
  #nombre de modalités de la cible, y est forcément un factor
  K <- nlevels(y)
  #modalités
  K_values <- levels(y)
  #Var.covar conditionnelles
  lst_Vk <- lapply(K_values,function(k){m_k <- as.matrix(X[y==k,]);(nrow(m_k)-1)*cov(m_k)})
  #matrice W - cov. intra-classes -- biaisée
  W <- Reduce("+",lst_Vk)/(n)
  #matrice V - cov. totale
  V <- (n-1)/n*cov(X)

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
      if(verbose){
        print("Plus de variables candidates dispo.")
        print("Fin du processus")
        cat("\n")
      }
      break
    }
    if(verbose){
      cat(paste("Etape :",(q+1)),"\n")
    }
    #préparer la structure pour récupérer les résultats
    curRes <- matrix(0,nrow=length(candidatsVars),ncol=3)
    rownames(curRes) <- candidatsVars
    colnames(curRes) <- c("Lambda","F","p-value")
    #pour chaque variable candidate
    for (v in candidatsVars){
      #ajouter la variable candidate à la liste courante
      tmpVars <- c(curVars,v)
      #former les matrices intermédiaires
      tmpW <- as.matrix(W[tmpVars,tmpVars])
      tmpV <- as.matrix(V[tmpVars,tmpVars])
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
    if(verbose){
      print(curRes)
    }
    #récupérer la meilleure variable (la 1ere puisque triée)... si significative
    if (curRes[1,"p-value"] < slentry){
      #nom de la variable sélectionnée
      theBest <- rownames(curRes)[1]
      if(verbose){
        print(paste("==> Variable introduite dans le modèle :",theBest))
      }
      #ajouter la variable dans le pool des sélectionnées
      curVars <- c(curVars,theBest)
      #supprimer cette variable de la sélection
      candidatsVars <- candidatsVars[candidatsVars != theBest]
      #màj du nb de var. dans le pool des sélectionnées
      q <- q + 1
      #màj de lambda
      curLambda <- curRes[1,"Lambda"]
    } else {
      if(verbose){
        print("Aucune variable ne répond au critère de sélection")
        print("Fin du processus")
      }
      #this is the end....
      break
    }
  }

  ########
  #sortie#
  ########


  newX <- data.frame(y=y, data[curVars])
  yname <- all.vars(formula)[1]
  colnames(newX)[1] <- yname
  newX

  return(newX)
}
