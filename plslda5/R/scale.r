#' Scaling a Data Frame or a Numerci Vector
#'
#' This function performs scaling on a given data frame or numeric vector. The data frame will be centered and
#' standardized if \code{reduce = T}.
#'
#' @usage
#' plsda.scale(X,reduce = F)
#' @param
#' X The matrix that must be centered and standardized if \code{reduce = T}.
#' @param
#' reduce is an optional parameters which if \code{TRUE} standardizes the data frame or numeric vector X.
#' @return
#' The function returns a list of at least the following components :
#' @return
#' \code{New} is the resulting centered (and standardized if \code{reduce = T}) matrix.
#' \cr
#' \code{means} is the average of the initial matrix or numeric vector X.
#' \cr
#' \code{vars} is the variance of the initial matrix or numeric vector X.
#' @examples
#'scale.t1<-plsda.scale(iris[,-5])
#'scale.t2<-plsda.scale(iris[,-5],reduce=T)

plsda.scale<-function(X, center=TRUE, scale=TRUE ){

  X<-as.matrix(X)
  if(typeof(X)!="double"){
    stop("La matrice en entrée doit contenir que des champs numériques")
  }


  #CENTRER
  if (is.logical(center)) {
    if (center) {
      X<-t(apply(X,1,function(x){ return(x-apply(X,2,mean))}))
      print(system.time(t(apply(X,1,function(x){ return(x-apply(X,2,mean))}))))
      print(system.time(sweep(X,1,'-', STATS = colMeans(X))))
      print(X)
      print(sweep(X,1,'-', STATS = colMeans(X)))

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

  #renvoyer sous forme de dataframe
  return(as.data.frame(X))

}



