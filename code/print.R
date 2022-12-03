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

print.PLSDA <- function(object){
  #affichage de la fonction de classement 
  cat("Fonction de classement des X obtenue par la combinaisaon PLS-LDA : ", object$coef_cte,"\n")
  #affichage des vecteurs latent de X"
  cat("Vecteurs latents de X :", object$comp_X)
}
