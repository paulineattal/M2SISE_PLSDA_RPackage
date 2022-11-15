

#surcharge de summary
summary.PLSDA <- function(object,ncomp=2){
  #vérification du nombre de composantes
  if (is.null(ncomp) || ncomp <= 0 || ncomp > length(object$vp))
  {
    ncomp = min(2,length(object$Xscores))
  }
  #affichage 
  
  
  #voir td senace 5
  
}