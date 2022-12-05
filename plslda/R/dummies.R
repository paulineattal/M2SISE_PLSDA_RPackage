#' plslda.dummies,
#' Converting factor to dummy
#'
#' @param
#' y the dataframe to convert into dummies
#' @param
#' mod parameter is set by default to NA, if the mod parameter is given, it is used in cross validation to calculate the PREdicted Sum Square (PRESS)
#' in a way that it allows the matrix of test data and the matrix of predicted values to have the same number and order of classes of the target variable
#' so that calculating the PRESS becomes possible.
#'
#' @return
#' It returns a dataframe, with factors converted to dummies (categorical variable converted to numeric (0,1))
#' @export
#'
#' @examples
#' data(iris)
#' formule = Species~.
#'
#' y <- as.factor(model.response(model.frame(formula=formule, data = iris)))
#'
#' dummy <- plslda.dummies(iris$Species, mod=NA)
#' dummy_mod <-plslda.dummies(iris$Species, mod= y)



plslda.dummies<-function(y, mod=NA){

  #formatage des entrées
  y <- as.factor(as.vector(y))
  mod <- as.factor(as.vector(mod))

  #si on a renseigné le paramètre mod
  if (!any(is.na(mod))){
    mod <- as.factor(as.vector(mod))
    n.mod <- levels(mod)
  } else {
    n.mod <- levels(y)
  }

  #matrice d'indicatrice 0/1
  dum<-sapply(n.mod,function(x){ifelse(y==x,1,0)})

  ########
  #sortie#
  ########

  #renvoyer sous la forme de data frame
  return(as.data.frame(dum))
}
