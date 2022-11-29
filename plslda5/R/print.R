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
  #affichage des scores de X 
  cat("Scores de X : ", object$Xscores,"\n")
  #affichage des poids de X
  cat("poids de X : ",object$Xloading.weights, "\n")
  #affichage des vecteurs latent de X"
  cat("Vecteurs latents :", object$Xloadings)
}
