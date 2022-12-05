#' print.PLSDA,
#' Function used to print classification function and latent vectors
#'
#' @param
#' a plslda object
#'
#' @return
#' prints the classification function of matrix X which is obtained by the combination PLS-LDA method
#' @return
#' prints the latent vectors of matrix x
#' @export
#'
#' @examples
#' data(iris)
#' formule = Species~.
#'
#' PLSDA_object = fit(formula=formule, data=iris)
#' print(PLSDA_object)
#'

print.PLSDA <- function(object, ...){

  #paramètre object
  if (class(object)!="PLSDA") {
    stop("Erreur : Object n'est pas un objet de type PLSDA")
  }

  #affichage de la fonction de classement
  cat("Fonction de classement des X obtenue par la combinaisaon PLS-LDA : ")
  cat("\n")
  print(object$coef_cte)
  cat("\n")
  #affichage des vecteurs latent de X"
  cat("Vecteurs latents de X : ")
  cat("\n")
  print(object$comp_X)
}
