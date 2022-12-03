
report.plslda <- function(y, ypred){

  dp=2
  cm <- table(y, ypred)
  ct <- sum(cm)
  cs <- colSums(cm)
  rs <- rowSums(cm)
  tp <- diag(cm)
  tn <- ct - (rs + cs - tp)
  fp <- rs - tp
  fn <- cs - tp
  pr <- tp / (tp + fp)
  re <- tp / (tp + fn)
  f1 <- 2 * pr * re / (pr + re)
  ac <- sum(tp) / ct
  
  summary <- round(data.frame(tp, tn, fp, fn, precision=pr, recall=re, f1_score=f1, support=cs), dp)
  accuracy <- round(ac, dp)
  res <- list("summary" = summary,
              "accuracy" = accuracy)
  
  return(res)
}