#print.R

#' This function prints the scores of
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
#'
#'
#'
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
