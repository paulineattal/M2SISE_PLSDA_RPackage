
plslda.metrics <- function(y, ypred){
  
  ###########################
  #vérifications des entrées#
  ###########################
  
  if(length(y) != length(ypred)){
    stop("Erreur : y et ypred n'ont pas le mêmes nombre de lignes")
    }
  
  #####################
  #Calculs des métrics#
  #####################

  #matrice de confusion
  lvls <- levels(y)
  cm <- table(factor(y,levels = lvls), factor(ypred, levels = lvls))
  
  ct <- sum(cm)
  cs <- colSums(cm)
  rs <- rowSums(cm)
  #vrais positifs
  tp <- diag(cm)
  #vrais negatifs
  tn <- ct - (rs + cs - tp)
  #faux positifs
  fp <- rs - tp
  #faux negatifs
  fn <- cs - tp
  #precision
  pr <- tp / (tp + fp)
  #rappel
  re <- tp / (tp + fn)
  #f1-score
  f1 <- 2 * pr * re / (pr + re)
  #accuracy
  ac <- sum(tp) / ct
  
  #résultats arondis à 2 chiffres significatifs
  summary <- round(data.frame(tp, tn, fp, fn, precision=pr, recall=re, f1_score=f1), 2)
  accuracy <- round(ac, 2)
  
  ##################################
  #stockage des résultats de sortie#
  ##################################
  
  res <- list("summary" = summary,
              "accuracy" = accuracy)
  
  return(res)
}