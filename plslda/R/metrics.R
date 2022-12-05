#' report.plslda,
#' function to calculate the different metrics of a prediction, to define the quality of the prediction
#' based on the confusion matrix components, and the accuracy score
#'
#' @param
#' y the values of test data
#' @param
#' ypred the predicted values
#'
#' @return
#' It returns :
#' a 'summary' dataframe which contains the information of the confusion matrix as in the true positive, true negative, false positive, false negative
#' the precision, recall and f1-score
#' an 'accuracy' variable which gives the accuracy of the prediction
#' @export
#'
#' @examples
#' data(iris)
#' formula = Species~.
#'
#' data_split = plslda.split_sample(formula=formule, data=iris)
#'
#' object = plslda.fit(formula=formula, data=data_split$train)
#'
#' ypred = plslda.predict(object=object,newdata=data_split$Xtest)
#'
#' metrics <- plslda.metrics(y=data_split$ytest, ypred=ypred)
#' print(metrics$summary)
#' print(metrics$accuracy)
#'


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
