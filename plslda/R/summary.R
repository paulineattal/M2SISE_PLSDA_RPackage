# surcharge de summary

#' summary.PLSDA,
#' This function prints the standardized projection coefficients of matrix X
#' @param
#' a plsda object
#'
#' @return
#' prints the standardized (centered and reduce) projection coefficients  of matrix X
#' @return
#' prints the restitution quality of the pls regression
#' @export
#'
#' @examples
#' data(iris)
#' formule = Species~.
#'
#' PLSDA_object = fit(formula=formule, data=iris)
#' summary(PLSDA_object)
#'

#surcharge de summary
summary.PLSDA <- function(object, ...){

  #paramètre object
  if (class(object)!="PLSDA") {
    stop("Erreur : Object n'est pas un objet de type PLSDA")
  }

  #affichage des coefficients de projection standardisés
  cat("Coefficients de projection standardisés : ")
  cat("\n")
  print(object$poid_X)
  cat("\n")
  #affichage de la qualité de restitution de la regression PLS
  cat("Qualité de restitution - Régression PLS : ")
  cat("\n")
  print(object$quality)

}
