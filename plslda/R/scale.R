#' plslda.scale,
#' Function to standardize vectors and matrixes (center and reduce)
#'
#' @param
#' X the given matrix must contain numeric values
#' @param
#' center set to TRUE by default, in order to center the matrix data
#' @param
#' scale set to TRUE by default, in order to reduce the matrix data
#'
#' @return
#' It returns a dataframe which is the result of the centered and reduced matrix, standardized data
#' @export
#'
#' @examples
#' data(iris)
#' scale.t1<-plslda.scale(iris[,-5],center=TRUE, scale=TRUE)
#' scale.t2<-plslda.scale(iris[,-5],center=TRUE, scale=FALSE)

plslda.scale<-function(X, center=TRUE, scale=TRUE ){

  X<-as.matrix(X)

  ###########################
  #vérifications des entrées#
  ###########################

  #paramètre X
  if(typeof(X)!="double"){
    stop("La matrice en entrée doit contenir que des champs numériques")
  }


  #CENTRER
  if (is.logical(center)) {
    if (center) {
      X<-t(apply(X,1,function(x){ return(x-apply(X,2,mean))}))
    }
  }
  else if (is.numeric(center) && (length(center) == ncol(x)))
    X<-t(apply(X,1,function(x){ return(x-center)}))


  #REDUIRE
  if (is.logical(scale)) {
    if (scale) {
      X <- t(apply(X,1,function(x){x/sqrt(apply(X,2,var))}))
    }
  }
  else if (is.numeric(scale) && length(scale) == ncol(X))
    X <- t(apply(X,1,function(x){x/scale}))

  ########
  #sortie#
  ########

  #renvoyer sous forme de dataframe
  return(as.data.frame(X))

}
